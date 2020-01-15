module Release
  ( release
  )
where

import           Data.Foldable                  ( for_ )
import           Data.Function                  ( (&) )
import           Data.Text                     as Text
                                                ( stripEnd
                                                , unwords
                                                , unlines
                                                , replace
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           GHC.Base                       ( NonEmpty(..) )
import           Prelude                 hiding ( FilePath
                                                , unlines
                                                , unwords
                                                )

import qualified Control.Foldl                 as L

import           Turtle                  hiding ( rm )

import qualified Data.Yaml                     as Yaml

import           Git
import           K8s.Resource
import           ReleaseConfig
import           Render

writeRenderedResource :: GitContext IO -> EnvironmentResource -> IO ()
writeRenderedResource git EnvironmentResource { path, resource } = do
  git $ rm [directory path]
  let outputPath = releaseOutputDir </> path
  mktree (directory outputPath)
  output outputPath (select (resourceToLines resource))

release :: FilePath -> IO ()
release manifestPath = do
  versionPath <- fromText <$> requireEnv "VERSION_FILE"
  version     <- stripEnd <$> readTextFile versionPath
  style       <- ReleaseConfig.manifestStyle manifestPath
  git         <- clone (format fp releaseInputDir) releaseOutputDir
  appName     <- case style of
    Boclips Bomanifest { app } -> pure (fromText app)

  case style of
    Boclips manifest -> for_ (outputResources manifest (Version version))
                             (writeRenderedResource git)

  git . add $ ["."]

  code <- releaseOutputDir `getExitCode` noChanges
  case code of
    ExitSuccess   -> echo "Nothing to commit to release manifests repo"
    ExitFailure _ -> do
      let foldFromSource gitCmd =
            getGit (fromText "source") gitCmd `fold` L.list
      remoteUrl <- foldFromSource (configGet "remote.origin.url")
      timestamp <- foldFromSource showCurrentAuthorTimestamp
      sourceSHA <- foldFromSource (showRefs ["HEAD", "--pretty=format:%H", "--no-patch"])
      git
        $  commit
             (commitMessage appName (linesToText sourceSHA) (stripEnd (linesToText remoteUrl)) version)
        <> ["--date=" <> linesToText timestamp]

commitMessage :: FilePath -> Text -> Text -> Text -> Text
commitMessage appName sourceSHA remoteUrl version = unlines
  [ unwords ["releasing:", format fp appName, prefixedVersion]
  , ""
  , "from: " <> sourceSHA
  , sshToHTTPS remoteUrl <> "/tree/" <> prefixedVersion
  ]
 where
  prefixedVersion = "v" <> version
  sshToHTTPS sshUrl =
    sshUrl & replace ".git" "" & replace "git@github.com:" "https://github.com/"

resourceToLines :: Resource -> NonEmpty Line
resourceToLines = textToLines . decodeUtf8 . Yaml.encode

releaseInputDir :: FilePath
releaseInputDir = fromText "release-manifests"

releaseOutputDir :: FilePath
releaseOutputDir = fromText "release-manifests-modified"

requireEnv :: Text -> IO Text
requireEnv envName = do
  val <- need envName
  case val of
    Nothing     -> die $ "Must set " <> envName
    Just envVal -> pure envVal
