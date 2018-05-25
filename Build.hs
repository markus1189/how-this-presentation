#! /usr/bin/env nix-shell
-- #! nix-shell deps.nix -i "ghci -fdefer-type-errors"
#! nix-shell shell.nix -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall"
#! nix-shell --pure
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Lens.Operators ((^.))
import           System.Directory (createDirectoryIfMissing, copyFile)
import           Data.Foldable (for_)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified Dhall as D
import qualified Data.ByteString.Lazy as BL
import           Control.Monad.IO.Class (MonadIO)
import qualified Network.Wreq as Wreq
import qualified System.IO as IO
import           Text.LaTeX
import qualified Data.Text as TS
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax

cmdOpts :: [CmdOption]
cmdOpts = [WithStdout True, EchoStdout False, EchoStderr False, Stdin ""]

data ImageSrc = ImageSrc { url :: Text, transformations :: [Text] } deriving (Show, D.Generic)
instance D.Interpret ImageSrc

data SnippetSrc = SnippetSrc { snippetFile :: Text, snippetStart :: Text, snippetEnd :: Text } deriving (Show, D.Generic)
instance D.Interpret SnippetSrc

newtype ScalaOptions = ScalaOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult ScalaOptions = [String]

newtype ScalafmtOptions = ScalafmtOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult ScalafmtOptions = [String]

newtype HindentOptions = HindentOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult HindentOptions = [String]

newtype DitaaOptions = DitaaOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult DitaaOptions = [String]

newtype GraphvizOptions = GraphvizOptions () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GraphvizOptions = [String]

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
  downloadResource <- addResource
  addOracles
  projectCompiler <- addProjectCompiler
  wantTargets
  phonyCommands
  rules projectCompiler downloadResource

wantTargets :: Rules ()
wantTargets = do
  want [buildDir </> "slides.pdf"]

phonyCommands :: Rules ()
phonyCommands = do
  phony "clean" (removeFilesAfter buildDir ["//*"])

addResource :: Rules Resource
addResource = newResource "Download" 10

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
  _ <- addOracle $ \(HindentOptions _) -> return ["--line-length", "55"]
  _ <- addOracle $ \(DitaaOptions _) -> return ["--scale"
                                               ,"4"
                                               ,"--overwrite"
                                               ]
  _ <- addOracle $ \(GraphvizOptions _) -> return ["-Tpng"
                                                  ,"-Gdpi=300"
                                                  ]
  return ()

addProjectCompiler :: Rules (() -> Action ())
addProjectCompiler = do
  newCache $ \() -> cmd_ [Cwd "source-code"] bin args
  where bin = "sbt" :: String
        args = ["compile"] :: [String]

includedFont :: FilePath
includedFont = buildDir </> "font.tex"

beamerThemes :: [FilePath]
beamerThemes = map (buildDir </>) ["beamercolorthemecodecentric.sty"
                                  ,"beamerfontthemecodecentric.sty"
                                  ,"beamerinnerthemecodecentric.sty"
                                  ,"beamerouterthemecodecentric.sty"
                                  ,"beamerthemecodecentric.sty"
                                  ]

rules :: (() -> Action ()) -> Resource -> Rules ()
rules projectCompiler downloadResource = do
  --snippet:pdf rule
  buildDir </> "slides.pdf" %> \out -> do
    let inp = out -<.> "tex"
    need (inp : includedFont : beamerThemes)
    latexmk inp
  --end:pdf rule

  buildDir </> "font.tex" %> \_ -> dumpFontFile

  buildDir </> "*.tex" %> \out -> do
    let inp = dropDirectory1 out
    needsCode <- codeDeps inp
    needsGraphics <- graphicDeps inp
    need (inp : needsCode ++ needsGraphics)
    liftIO (copyFile inp out)

  buildDir </> "*.sty" %> \out -> do
    copyFileChanged (dropDirectory1 out) out

  buildDir </> "snippets" </> "*.scala" %> \out -> do
    _ <- projectCompiler ()
    snip <- extractSnippet (dropDirectory1 $ out -<.> "snippet")
    writeFileChanged out snip
    checkScala out
    scalafmt out

  buildDir </> "snippets" </> "*.hs" %> \out -> do
    snip <- extractSnippet (dropDirectory1 $ out -<.> "snippet")
    writeFileChanged out snip
    hindent out

  buildDir </> "ditaa/*.png" %> \out -> do
    let inp = dropDirectory1 out -<.> "ditaa"
    need [inp]
    ditaa inp out

  buildDir </> "graphviz/*.png" %> \out -> do
    let inp = dropDirectory1 out -<.> "dot"
    need [inp]
    graphviz inp out

  buildDir </> "static-images/*" %> \out -> do
    let inp = dropDirectory1 out
    copyFileChanged inp out

  alternatives $ do
    buildDir </> "static-source/*.png" %> \out -> do
      let inp = dropExtension (dropDirectory1 out)
      need [inp]
      cmd [Stdin ""] ("pygmentize" :: String) ["-Ofont_name=Ubuntu Mono", "-o", out, inp]

    buildDir </> "static-source/*" %> \out -> do
      let inp = dropDirectory1 out
      copyFileChanged inp out

  [ buildDir </> "images/*" <.> ext | ext <- [ "jpg", "png", "gif" ] ] |%> \out -> do
    let inp = dropDirectory1 $ out -<.> "src"
    need [inp]
    ImageSrc uri ts <- traced "image-src" (readDhall inp)
    download downloadResource (TS.unpack uri) out
    for_ ts $ unit . applyTransformation out

graphviz :: FilePath -> FilePath -> Action ()
graphviz inp out = do
  opts <- askOracle (GraphvizOptions ())
  cmd cmdOpts bin (opts ++ ["-o", out, inp])
  where bin = "dot" :: String

ditaa :: FilePath -> FilePath -> Action ()
ditaa inp outp = do
  opts <- askOracle (DitaaOptions ())
  cmd cmdOpts bin ([inp, outp] ++ opts)
  where bin = "ditaa" :: String

latexmk :: FilePath -> Action ()
latexmk inp = do
  cmd (Cwd (takeDirectory inp) : cmdOpts)
      bin
      ["-g", "-shell-escape", "-pdfxe", dropDirectory1 inp]
  where bin = "latexmk" :: String

checkScala :: FilePath -> Action ()
checkScala inp = do
  opts <- askOracle (ScalaOptions ())
  cmd bin (opts ++ [inp])
  where bin = "scala" :: String

hindent :: FilePath -> Action ()
hindent inp = do
  opts <- askOracle (HindentOptions ())
  cmd bin (opts ++ [inp])
  where bin = "hindent" :: String

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
  deps <- map (buildDir </>) . filter (not . (`elem` ["scala", "yaml", "haskell"])) <$> commandDeps ["inputminted"] file
  return deps

extractSnippet :: FilePath -> Action String
extractSnippet file = do
  putQuiet ("Extracting from " <> file)
  SnippetSrc (T.unpack -> sourceFile) (T.unpack -> startString) (T.unpack -> endString) <- readDhall file
  lns <- readFileLines sourceFile
  let result = takeWhile (not . (endString `isPrefixOf`) . dropWhile (== ' '))
             . dropWhile (not . (startString `isPrefixOf`) . dropWhile (== ' '))
             $ lns
  if null result
    then error ("Empty snippet for:\n" <> file <> ":0:")
    else return (unlines (drop 1 result))

download :: Resource -> String -> FilePath -> Action ()
download res uri target = withResource res 1 $ traced "download" $ do
  createDirectoryIfMissing True (takeDirectory target)
  r <- Wreq.get uri
  BL.writeFile target (r ^. Wreq.responseBody)

readDhall :: (D.Interpret a, MonadIO m) => String -> m a
readDhall p = liftIO $ D.input D.auto ("./" <> TL.pack p)

applyTransformation :: String -> Text -> Action ()
applyTransformation out t = cmd [Stdin ""] bin (words (TS.unpack t) ++ [out, out])
  where bin = "convert" :: String
