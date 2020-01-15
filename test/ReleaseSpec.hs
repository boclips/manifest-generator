module ReleaseSpec
  ( spec
  )
where

import           Prelude                 hiding ( FilePath )
import           Test.Hspec
import           Turtle
import           Control.Exception              ( finally )

import           Git
import           Release                        ( release )
import           TestHelpers

spec :: Spec
spec = do
  context "first release" $ do
    describe "bo manifest app" $ do
      it "copies the repo author date to the release-manifests commit" $ do
        outputLines <- asList $ do
          (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
            "releasespec-render-cool-gang-app"
          git <- initialise oldReleaseManifests
          git (commit "root" <> ["--allow-empty"])

          sourceGit <- initialiseSource
            dir
            "git@github.com:boclips/cool-app-different-name.git"
          sourceGit
            $  commit "some work"
            <> ["--allow-empty", "--date='118675309 +0000'"]

          versionFile <- writeConcourseVersionFixture dir "1.2.3"
          export "VERSION_FILE" (format fp versionFile)

          let manifestPath = dir </> "mymanifest.yaml"
          output manifestPath (boManifestForAppName "cool-gang-app")
          runFromDir dir (release manifestPath)

          newReleaseManifests
            `getGit` showRefs ["HEAD", "--pretty=format:%at", "--no-patch"]

        outputLines `shouldBe` ["118675309"]

      it "includes data about the source commit in the release commit message" $ do
        (sha : message) <- asList $ do
          (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
            "releasespec-render-cool-gang-app"

          git <- initialise oldReleaseManifests
          git (commit "root" <> ["--allow-empty"])

          sourceGit <- initialiseSource
            dir
            "git@github.com:boclips/cool-app-different-name.git"
          sourceGit
            $  commit "some work"
            <> ["--allow-empty", "--date='118675309 +0000'"]

          sourceSHA <- dir </> "source" `getGit` showRefs ["HEAD", "--pretty=format:%H", "--no-patch"]

          versionFile <- writeConcourseVersionFixture dir "1.2.3"
          export "VERSION_FILE" (format fp versionFile)

          let manifestPath = dir </> "mymanifest.yaml"
          output manifestPath (boManifestForAppName "cool-gang-app")
          runFromDir dir (release manifestPath)

          pure sourceSHA +
            newReleaseManifests
            `getGit` showRefs ["HEAD", "--pretty=format:%B", "--no-patch"]

        message
          `shouldBe` [ "releasing: cool-gang-app v1.2.3"
                     , ""
                     , "from: " <> sha
                     , ""
                     , "https://github.com/boclips/cool-app-different-name/tree/v1.2.3"
                     ]

      it "renders k8s manifests for all environments" $ do
        let boManifest = select
              [ "app: bo-manifest-app"
              , "port: 3000"
              , "healthPath: /api/health"
              , "scale: ExtraLarge"
              , "bootTime: Slow"
              , "preStop:"
              , "- sh"
              , "- -c"
              , "- \"sleep 5\""
              , "envVarFromMountedSecret:"
              , "  envVar: GOOGLE_APPLICATION_CREDENTIALS"
              , "  mountedSecret: google-application-credentials"
              , "  key: key.json"
              , "environments:"
              , "  production:"
              , "    replicas: 987"
              , "    urls:"
              , "    - bo-manifest-app.boclips.com/v1"
              , "  staging:"
              , "    replicas: 1"
              , "    urls:"
              , "    - bo-manifest-app.staging-boclips.com/v1"
              , "  testing:"
              , "    replicas: 0"
              , "    urls:"
              , "    - bo-manifest-app.testing-boclips.com/v1"
              ]

        outputLines <- asList $ do
          (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
            "releasespec-render-bo-manifest"
          git <- initialise oldReleaseManifests

          touch (oldReleaseManifests </> "existing-file")
          git $ add ["."]
          git $ commit "existing-file"

          _           <- initialiseSource dir "git@github.com:boclips/some-url"

          versionFile <- writeConcourseVersionFixture dir "1.2.3"
          export "VERSION_FILE" (format fp versionFile)

          let manifestPath = dir </> "mymanifest.yaml"
          output manifestPath boManifest
          runFromDir dir (release manifestPath)

          newReleaseManifests `getGit` lsFiles

        outputLines
          `shouldMatchList` [ "existing-file"
                            , "testing/bo-manifest-app/ingress.yaml"
                            , "staging/bo-manifest-app/ingress.yaml"
                            , "production/bo-manifest-app/ingress.yaml"
                            , "testing/bo-manifest-app/service.yaml"
                            , "staging/bo-manifest-app/service.yaml"
                            , "production/bo-manifest-app/service.yaml"
                            , "testing/bo-manifest-app/deployment.yaml"
                            , "staging/bo-manifest-app/deployment.yaml"
                            , "production/bo-manifest-app/deployment.yaml"
                            ]

boManifestForAppName :: Line -> Shell Line
boManifestForAppName app = select
  [ "app: " <> app
  , "port: 3000"
  , "scale: Small"
  , "environments:"
  , "  production:"
  , "    replicas: 1"
  , "    urls:"
  , "    - " <> app <> ".boclips.com/v1"
  ]

initialiseSource :: MonadIO io => FilePath -> Text -> io (GitContext io)
initialiseSource dir remoteUrl = do
  sourceGit <- initialise $ dir </> "source"
  sourceGit $ config "remote.origin.url" remoteUrl
  sourceGit $ commit "root" <> ["--allow-empty"]
  pure sourceGit

writeConcourseVersionFixture :: MonadIO io => FilePath -> Line -> io FilePath
writeConcourseVersionFixture dir version = do
  let versionFile = dir </> "my-version-file"
  output versionFile (pure version)
  pure versionFile

tempDirNames :: Text -> Shell (FilePath, FilePath, FilePath)
tempDirNames name = do
  dir <- using (mktempdir "/tmp" name)
  pure (dir, dir </> "release-manifests", dir </> "release-manifests-modified")

runFromDir :: MonadIO io => FilePath -> IO a -> io a
runFromDir dir script = liftIO $ do
  cwd <- pwd
  cd dir
  script `finally` cd cwd
