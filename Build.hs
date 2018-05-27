#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i "runhaskell --ghc-arg=-threaded --ghc-arg=-Wall"
#! nix-shell --pure
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Control.Lens.Operators ((^.))
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import           Data.List (isPrefixOf)
import           Data.Maybe (mapMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified Dhall as D
import qualified Network.Wreq as Wreq
import           System.Directory (createDirectoryIfMissing, copyFile)
import qualified System.IO as IO
import           Text.LaTeX
import           Text.LaTeX.Base.Parser
import           Text.LaTeX.Base.Syntax

cmdOpts :: [CmdOption]
cmdOpts = [WithStdout True, EchoStdout False, EchoStderr False, Stdin ""]

data Addr = Start | Search { term :: Text} | End deriving (Show, D.Generic)
instance D.Interpret Addr

data ImageSrc = ImageSrc { url :: Text, transformations :: [Text] } deriving (Show, D.Generic)
instance D.Interpret ImageSrc

data SnippetSrc = SnippetSrc { snippetFile :: Text, snippetStart :: Addr, snippetEnd :: Addr } deriving (Show, D.Generic)
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
  sbtCompile <- addProjectCompiler
  wantTargets
  phonyCommands
  rules sbtCompile downloadResource

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
addProjectCompiler = newCache $ \() -> cmd_ [Cwd "source-code"] "sbt" ["compile"]

includedFont :: FilePath
includedFont = buildDir </> "font.tex"

beamerThemes :: [FilePath]
beamerThemes = map (buildDir </>) ["beamercolorthemecodecentric.sty"
                                  ,"beamerfontthemecodecentric.sty"
                                  ,"beamerinnerthemecodecentric.sty"
                                  ,"beamerouterthemecodecentric.sty"
                                  ,"beamerthemecodecentric.sty"
                                  ]

createByCopy :: FilePath -> Rules ()
createByCopy ptrn = buildDir </> ptrn %> \out -> copyFileChanged (dropDirectory1 out) out

rules :: (() -> Action ()) -> Resource -> Rules ()
rules sbtCompile downloadResource = do
  --snippet:outer pdf rule
  --snippet:pdf rule
  buildDir </> "slides.pdf" %> \out -> do
    let inp = out -<.> "tex"
    need (inp : includedFont : beamerThemes)
    latexmk inp
  --end:pdf rule
  --end:outer pdf rule

  buildDir </> "font.tex" %> \_ -> dumpFontFile

  buildDir </> "*.tex" %> \out -> do
    let inp = dropDirectory1 out
    needsCode <- codeDeps inp
    needsGraphics <- graphicDeps inp
    need (inp : needsCode ++ needsGraphics)
    chktex inp
    liftIO (copyFile inp out)

  createByCopy "*.sty"

  buildDir </> "snippets" </> "*.scala" %> \out -> do
    _ <- sbtCompile ()
    handleSnippet out $ \file -> do
      checkScala file
      scalafmt out

  --snippet:outer hs snippet rule
  --snippet:hs snippet rule
  buildDir </> "snippets" </> "*.hs" %> \out -> do
    snip <- extractSnippet (dropDirectory1 $ out -<.> "snippet")
    withTempFile $ \temp -> do
      liftIO (writeFile temp snip)
      hlint temp
      hindent temp
      content <- liftIO (readFile temp)
      writeFileChanged out content
  --end
  --end:outer hs snippet rule

  buildDir </> "snippets" </> "*.hs_noformat" %> \out -> do
    handleSnippet out hlint

  buildDir </> "snippets" </> "*.yml" %> \out -> do
    handleSnippet out (void . return)

  buildDir </> "snippets" </> "*.snippet" %> \out -> do
    withTempFile $ \temp -> do
      content <- liftIO (readFile (dropDirectory1 $ out))
      liftIO (writeFile temp content)
      dhallFormat temp
      content <- liftIO (readFile temp)
      writeFileChanged out content

  buildDir </> "ditaa/*.png" %> \out -> do
    let inp = dropDirectory1 out -<.> "ditaa"
    need [inp]
    ditaa inp out

  createByCopy "ditaa/*.ditaa"

  --snippet:graphviz rule
  buildDir </> "graphviz/*.png" %> \out -> do
    let inp = dropDirectory1 out -<.> "dot"
    need [inp]
    graphviz inp out
  --end

  createByCopy "graphviz/*.dot"

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

  --snippet:download-images
  [ buildDir </> "images/*" <.> ext | ext <- [ "jpg", "png", "gif" ] ] |%> \out -> do
    let inp = dropDirectory1 $ out -<.> "src"
    need [inp]
    ImageSrc uri ts <- traced "image-src" (readDhall inp)
    download downloadResource (TS.unpack uri) out
    for_ ts $ unit . applyTransformation out
  --end

  createByCopy "images/*.src"

dhallFormat :: FilePath -> Action ()
dhallFormat inp = cmd cmdOpts "dhall-format" ["--inplace", inp]

chktex :: FilePath -> Action ()
chktex inp = cmd cmdOpts "chktex" inp

hlint :: FilePath -> Action ()
hlint inp = do
  cmd cmdOpts "hlint" inp

graphviz :: FilePath -> FilePath -> Action ()
graphviz inp out = do
  opts <- askOracle (GraphvizOptions ())
  cmd cmdOpts "dot" (opts ++ ["-o", out, inp])

ditaa :: FilePath -> FilePath -> Action ()
ditaa inp outp = do
  opts <- askOracle (DitaaOptions ())
  cmd cmdOpts "ditaa" ([inp, outp] ++ opts)

latexmk :: FilePath -> Action ()
latexmk inp = do
  cmd (Cwd (takeDirectory inp) : cmdOpts)
      "latexmk"
      ["-g", "-shell-escape", "-pdfxe", dropDirectory1 inp]

checkScala :: FilePath -> Action ()
checkScala inp = do
  opts <- askOracle (ScalaOptions ())
  cmd "scala" (opts ++ [inp])

hindent :: FilePath -> Action ()
hindent inp = do
  opts <- askOracle (HindentOptions ())
  cmd "hindent" (opts ++ [inp])

scalafmt :: FilePath -> Action ()
scalafmt inp = do
  opts <- askOracle (ScalafmtOptions ())
  contents <- liftIO (IO.readFile inp)
  withTempFile $ \temp -> do
    let wrapped = unlines $ "object ObjForScalafmt {" : lines contents ++ ["}"]
    liftIO $ IO.writeFile temp wrapped
    Stdout stdout <- cmd [EchoStdout False, EchoStderr False] "scalafmt" (opts ++ [temp])
    let output = unlines (init (drop 1 (lines stdout)))
    liftIO $ IO.writeFile inp output

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

commandDeps :: [String] -> FilePath -> Action [[FilePath]]
commandDeps cmds file = do
  etex <- liftIO (parseLaTeXFile file)
  case etex of
    Left err -> error ("Parsing of file " <> file <> " failed: " <> show err)
    Right t -> do
      let result = map (map T.unpack . mapMaybe cmdArgs) .
                   map snd .
                   matchCommand (`elem` cmds) $
                   t
      return result

graphicDeps :: FilePath -> Action [FilePath]
graphicDeps file = map ((buildDir </>) . concat) <$> commandDeps ["includegraphics"] file

codeDeps :: FilePath -> Action [FilePath]
codeDeps file = map (buildDir </>) . concatMap (drop 1) <$> commandDeps ["inputminted"] file

extractSnippet :: FilePath -> Action String
extractSnippet file = do
  putQuiet ("Extracting from " <> file)
  need [file]
  SnippetSrc (T.unpack -> sourceFile) startSearch endSearch <- readDhall file
  lns <- readFileLines sourceFile
  let result = findSnippet startSearch endSearch lns
  if null result
    then error ("Empty snippet for:\n" <> file <> ":0:")
    else return (unlines result)

findSnippet :: Addr -> Addr -> [String] -> [String]
findSnippet (Search (T.unpack -> startString)) (Search (T.unpack -> endString)) lns =
  drop 1
  . takeWhile (not . (endString `isPrefixOf`) . dropWhile (== ' '))
  . dropWhile (not . (startString `isPrefixOf`) . dropWhile (== ' '))
  $ lns
findSnippet (Search (T.unpack -> startString)) End lns =
  drop 1 . dropWhile (not . (startString `isPrefixOf`) . dropWhile (== ' ')) $ lns
findSnippet Start (Search (T.unpack -> endString)) lns =
  takeWhile (not . (endString `isPrefixOf`) . dropWhile (== ' ')) lns
findSnippet Start End lns = lns
findSnippet s e _ = error $ "invalid combination of addresses: " ++ show (s,e)

handleSnippet :: FilePath -> (FilePath -> Action ()) -> Action ()
handleSnippet out act = do
  snip <- extractSnippet (dropDirectory1 $ out -<.> "snippet")
  withTempFile $ \temp -> do
    liftIO (writeFile temp snip)
    act temp
    content <- liftIO (readFile temp)
    writeFileChanged out content

download :: Resource -> String -> FilePath -> Action ()
download res uri target = withResource res 1 $ traced "download" $ do
  createDirectoryIfMissing True (takeDirectory target)
  r <- Wreq.get uri
  BL.writeFile target (r ^. Wreq.responseBody)

readDhall :: (D.Interpret a, MonadIO m) => String -> m a
readDhall p = liftIO $ D.input D.auto (TL.pack $ "./" <> p)

applyTransformation :: String -> Text -> Action ()
applyTransformation out t = cmd [Stdin ""] "convert" (words (TS.unpack t) ++ [out, out])
