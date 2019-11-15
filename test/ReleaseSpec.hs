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

    it "renders manifests in the cloned release manifest repo" $ do
      outputLines <- asList $ do
        (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
          "releasespec-app-first-release"

        inputManifestsDir <- createRawK8sAppRepo dir "new-app" ["{}"]
        git               <- initialise oldReleaseManifests
        git (commit "root" <> ["--allow-empty"])

        _           <- initialiseSource dir "git@github.com:boclips/some-url"

        versionFile <- writeConcourseFixtures dir "new-app" "1.2.3"
        export "INPUT_MANIFESTS_DIR" (format fp inputManifestsDir)
        export "VERSION_FILE"        (format fp versionFile)
        export "PRODUCTION_REPLICAS" "5"
        export "STAGING_REPLICAS"    "2"
        runFromDir dir (release "unused.yaml")

        newReleaseManifests `getGit` lsFiles

      outputLines
        `shouldMatchList` [ "production/new-app/deployment.yaml"
                          , "production/new-app/ingress.yaml"
                          , "production/new-app/service.yaml"
                          , "staging/new-app/deployment.yaml"
                          , "staging/new-app/ingress.yaml"
                          , "staging/new-app/service.yaml"
                          , "testing/new-app/deployment.yaml"
                          , "testing/new-app/ingress.yaml"
                          , "testing/new-app/service.yaml"
                          ]

  context "when the app's manifest has changed"
    $ it "commits to the cloned release manifest repo with replaced vars"
    $ do
        outputLines <- asList $ do
          (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
            "releasespec-app-manifest-change"

          inputManifestsDir <- createRawK8sAppRepo
            dir
            "existing-app"
            ["newReplicas: $REPLICAS", "version: \"myimage:${VERSION}\""]

          git <- initialise oldReleaseManifests
          mktree (oldReleaseManifests </> "testing/existing-app")
          output
            (oldReleaseManifests </> "testing/existing-app/deployment.yaml")
            "{oldReplicas: 1, version: myimage:1.2}"
          git . add $ ["."]
          git . commit $ "Existing App 1.2"

          _           <- initialiseSource dir "git@github.com:boclips/some-url"

          versionFile <- writeConcourseFixtures dir "existing-app" "1.2"
          export "INPUT_MANIFESTS_DIR" (format fp inputManifestsDir)
          export "VERSION_FILE"        (format fp versionFile)
          export "STAGING_REPLICAS"    "123456"
          export "TESTING_REPLICAS"    "8675309"
          runFromDir dir (release "unused.yaml")

          newReleaseManifests `getGit` showRefs
            [ "HEAD:testing/existing-app/deployment.yaml"
            , "HEAD:staging/existing-app/deployment.yaml"
            ]

        outputLines
          `shouldBe` [ "newReplicas: 8675309"
                     , "version: \"myimage:1.2\""
                     , "newReplicas: 123456"
                     , "version: \"myimage:1.2\""
                     ]

  context "when neither the app's manifest nor its variables have changed"
    $ it "clones the release manifest repo with no new commits"
    $ do
        outputLines <- asList $ do
          (dir, oldReleaseManifests, newReleaseManifests) <- tempDirNames
            "releasespec-app-manifest-no-changes"

          inputManifestsDir <- createRawK8sAppRepo
            dir
            "unchanging-app"
            ["replicas: $REPLICAS", "version: $VERSION"]

          git <- initialise (dir </> "release-manifests")

          writeK8sFiles (oldReleaseManifests </> "testing/unchanging-app")
                        ["replicas: 999", "version: \"1.2\""]
          writeK8sFiles (oldReleaseManifests </> "staging/unchanging-app")
                        ["replicas: 2", "version: \"1.2\""]
          writeK8sFiles (oldReleaseManifests </> "production/unchanging-app")
                        ["replicas: 2", "version: \"1.2\""]

          git . add $ ["."]
          git . commit $ "Unchanging App 1.2"

          _           <- initialiseSource dir "git@github.com:boclips/some-url"

          versionFile <- writeConcourseFixtures dir "unchanging-app" "1.2"
          export "INPUT_MANIFESTS_DIR" (format fp inputManifestsDir)
          export "VERSION_FILE"        (format fp versionFile)
          export "PRODUCTION_REPLICAS" "2"
          export "STAGING_REPLICAS"    "2"
          export "TESTING_REPLICAS"    "999"
          runFromDir dir (release "unused.yaml")

          newReleaseManifests `getExitCode` noChanges

        last outputLines `shouldBe` ExitSuccess

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

writeK8sFiles :: MonadIO io => FilePath -> [Line] -> io ()
writeK8sFiles dir deploymentManifest = do
  mktree dir
  output (dir </> "deployment.yaml") (select deploymentManifest)
  output (dir </> "service.yaml")    "{}"
  output (dir </> "ingress.yaml")    "{}"

createRawK8sAppRepo
  :: MonadIO io => FilePath -> FilePath -> [Line] -> io FilePath
createRawK8sAppRepo dir appName deploymentTemplate = do
  let inputManifestsDir = dir </> appName </> "k8s"
  writeK8sFiles inputManifestsDir deploymentTemplate
  pure inputManifestsDir

writeConcourseFixtures :: MonadIO io => FilePath -> Line -> Line -> io FilePath
writeConcourseFixtures dir appName version = do
  mkdir (dir </> "image")
  output (dir </> "image/repository") (pure appName)
  writeConcourseVersionFixture dir version

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
