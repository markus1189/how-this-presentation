#! /usr/bin/env nix-shell
-- #! nix-shell deps.nix -i "ghci -fdefer-type-errors"
#! nix-shell shell.nix -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall"
#! nix-shell --pure
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Monad (unless)
import           Data.List (intercalate, isPrefixOf)
import           Data.List.Split (chunksOf)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Traversable (for)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified System.IO as IO
import           Text.LaTeX
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax

newtype ScalaOptions = ScalaOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult ScalaOptions = [String]

newtype ScalafmtOptions = ScalafmtOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult ScalafmtOptions = [String]

newtype DitaaOptions = DitaaOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult DitaaOptions = [String]

main :: IO ()
main = runShakeBuild

buildDir :: FilePath
buildDir = "result"

myShakeOptions :: ShakeOptions
myShakeOptions = shakeOptions { shakeLint = Just LintBasic
                              , shakeReport = ["report.html", "report.json"]
                              , shakeThreads = 0
                              , shakeColor = True
                              }

runShakeBuild :: IO ()
runShakeBuild = shakeArgs myShakeOptions $ do
  addOracles
  projectCompiler <- addProjectCompiler
  wantTargets
  phonyCommands
  rules projectCompiler

wantTargets :: Rules ()
wantTargets = do
  want [buildDir </> "slides.pdf"]

phonyCommands :: Rules ()
phonyCommands = do
  phony "clean" (removeFilesAfter buildDir ["//*"])

addOracles :: Rules ()
addOracles = do
  _ <- addOracle $ \(ScalaOptions _) -> return ["-Ystop-after:namer"
                                               ,"-feature"
                                               ,"-deprecation"
                                               ,"-language:higherKinds"
                                               ,"-Xlint"
                                               ,"-nocompdaemon"
                                               ]
  _ <- addOracle $ \(ScalafmtOptions _) -> return ["--non-interactive"
                                                  ,"--quiet"
                                                  ,"--no-stderr"
                                                  ,"--stdout"
                                                  ,"--config-str"
                                                  ,"maxColumn = 55"
                                                  ]
  _ <- addOracle $ \(DitaaOptions _) -> return ["--scale"
                                               ,"3"
                                               ,"--overwrite"
                                               ]
  return ()

addProjectCompiler :: Rules (() -> Action ())
addProjectCompiler = do
  newCache $ \() -> cmd [Cwd "source-code"] "sbt" ["compile"]

rules :: (() -> Action ()) -> Rules ()
rules projectCompiler = do
  buildDir </> "slides.pdf" %> \out -> do
    let inp = out -<.> "tex"
        theme = map (buildDir </>) ["beamercolorthemecodecentric.sty"
                                   ,"beamerfontthemecodecentric.sty"
                                   ,"beamerinnerthemecodecentric.sty"
                                   ,"beamerouterthemecodecentric.sty"
                                   ,"beamerthemecodecentric.sty"
                                   ]
    need (inp : (buildDir </> "font.tex") : theme)
    latexmk inp

  buildDir </> "font.tex" %> \_ -> dumpFontFile

  buildDir </> "*.tex" %> \out -> do
    let inp = dropDirectory1 out
    needsCode <- codeDeps inp
    needsGraphics <- graphicDeps inp
    need (needsCode ++ needsGraphics)
    copyFileChanged inp out

  buildDir </> "*.sty" %> \out -> do
    copyFileChanged (dropDirectory1 out) out

  buildDir </> "snippets" </> "*.scala" %> \out -> do
    _ <- projectCompiler ()
    snip <- extractSnippet (dropDirectory1 $ out -<.> "snippet")
    writeFileChanged out snip
    checkScala out
    scalafmt out

  buildDir </> "ditaa/*.png" %> \out -> do
    let inp = dropDirectory1 out -<.> "ditaa"
    ditaa inp out

  buildDir </> "static-images/*" %> \out -> do
    let inp = dropDirectory1 out
    copyFileChanged inp out

ditaa :: FilePath -> FilePath -> Action ()
ditaa inp outp = do
  opts <- askOracle (DitaaOptions ())
  cmd bin ([inp, outp] ++ opts)
  where bin = "ditaa" :: String

latexmk :: FilePath -> Action ()
latexmk inp = do
  cmd [Cwd (takeDirectory inp)
      ,WithStdout True
      ,EchoStdout False
      ,EchoStderr False
      ,Stdin ""
      ] bin ["-g", "-shell-escape", "-pdfxe", dropDirectory1 inp]
  where bin = "latexmk" :: String

checkScala :: FilePath -> Action ()
checkScala inp = do
  opts <- askOracle (ScalaOptions ())
  cmd bin (opts ++ [inp])
  where bin = "scala" :: String

scalafmt :: FilePath -> Action ()
scalafmt inp = do
  opts <- askOracle (ScalafmtOptions ())
  contents <- liftIO (IO.readFile inp)
  withTempFile $ \temp -> do
    let wrapped = unlines $ "object ObjForScalafmt {" : lines contents ++ ["}"]
    liftIO $ IO.writeFile temp wrapped
    Stdout stdout <- cmd [EchoStdout False, EchoStderr False] bin (opts ++ [temp])
    let output = unlines (init (drop 1 (lines stdout)))
    liftIO $ IO.writeFile inp output
  where bin = "scalafmt" :: String

dumpFontFile :: Action ()
dumpFontFile = do
  putQuiet ("Dumping font file to " ++ (buildDir </> "font.tex"))
  -- Guaranteed to be present via `shell.nix`, although this couples shake and nix...

  Just useCodecentricFont <- getEnv "USE_CC_FONT"
  let filename = if useCodecentricFont == "true" then "font_cc.tex" else "font_non_cc.tex"
      outname = (buildDir </> "font.tex")
  copyFile' filename outname

cmdArgs :: TeXArg -> Maybe Text
cmdArgs (FixArg (TeXRaw arg)) = Just arg
cmdArgs _ = Nothing

commandDeps :: [String] -> FilePath -> Action [FilePath]
commandDeps cmds file = do
  etex <- liftIO (parseLaTeXFile file)
  case etex of
    Left err -> error ("Parsing of file " <> file <> " failed: " <> show err)
    Right t -> do
      let result = map T.unpack .
                   mapMaybe cmdArgs .
                   concatMap snd .
                   matchCommand (`elem` cmds) $
                   t
      return result

graphicDeps :: FilePath -> Action [FilePath]
graphicDeps file = map (buildDir </>) <$> commandDeps ["includegraphics"] file

codeDeps :: FilePath -> Action [FilePath]
codeDeps file = do
  deps <- map (buildDir </>) . filter (/= "scala") <$> commandDeps ["inputminted"] file
  putQuiet ("Discovered dependencies for '" <> file <> "': " <> show deps)
  return deps

extractSnippet :: FilePath -> Action String
extractSnippet file = do
  putQuiet ("Extracting from " <> file)
  snippets <- filter ((==3) . length) . chunksOf 3 <$> readFileLines file
  fmap concat . for snippets $ \ls -> do
    unless (length ls == 3) $
      error ("Error when reading snippet " ++ file
          ++ " need exactly three lines but got " ++ show (length ls) ++ ":\n"
          ++ intercalate "\n"
                         (zipWith (\i line -> show i ++ ": " ++ line) [(1::Int)..] ls))
    let [sourceFile,startString,endString] = ls
    lns <- readFileLines sourceFile
    let result = takeWhile (not . (endString `isPrefixOf`) . dropWhile (== ' '))
               . dropWhile (not . (startString `isPrefixOf`) . dropWhile (== ' '))
               $ lns
    if null result
      then error ("Empty snippet for:\n" <> file <> ":0:")
      else return (unlines (drop 1 result))
