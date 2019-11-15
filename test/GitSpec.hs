module GitSpec
  ( spec
  )
where

import           Test.Hspec
import           Turtle

import           TestHelpers

import           Git

spec :: Spec
spec = describe "Git" $ do
  it "can clone and commit to the cloned repo" $ do
    outputLines <- asList $ do
      dir <- using (mktempdir "/tmp" "gitspec")
      let srcRepo = dir </> "source-repo"
      let newRepo = dir </> "cloned-repo"

      sh $ do
        git <- initialise srcRepo
        touch $ srcRepo </> "origfile"
        git . add $ ["."]
        git . commit $ "Initial commit"

      sh $ do
        git <- clone (format fp srcRepo) newRepo
        touch $ newRepo </> "somefile"
        git . add $ ["."]
        git . commit $ "Some message"

      newRepo `getGit` status

    outputLines
      !!         1
      `shouldBe` "Your branch is ahead of 'origin/master' by 1 commit."

  it "reports when there are no changes to commit" $ do
    outputLines <- asList $ do
      dir <- using (mktempdir "/tmp" "gitspec")
      let repo = dir </> "source-repo"

      sh $ do
        git <- initialise repo
        touch $ repo </> "something"
        git . add $ ["."]
        git . commit $ "Initial commit"

      repo `getExitCode` noChanges

    outputLines `shouldBe` [ExitSuccess]

  it "reports when there are some changes to commit" $ do
    outputLines <- asList $ do
      dir <- using (mktempdir "/tmp" "gitspec")
      let repo = dir </> "source-repo"

      sh $ do
        git <- initialise repo
        touch $ repo </> "something"
        git . add $ ["."]
        git . commit $ "Initial commit"
        touch $ repo </> "something else"
        git . add $ ["."]

      repo `getExitCode` noChanges

    outputLines `shouldBe` [ExitFailure 1]
