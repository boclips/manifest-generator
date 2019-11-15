module EnvParseSpec
  ( spec
  )
where

import qualified Data.Text                     as T
import           Data.Text.Arbitrary            ( )
import           Test.Hspec
import           Test.QuickCheck
import           Turtle

import           EnvParse

spec :: Spec
spec = do
  it "defaults variables that aren't set" $ do
    val <- defaultingEnv "NOT_SET_PLZ" "mydefault"
    val `shouldBe` "mydefault"

  it "defaults variables that are empty"
    $ property
    $ forAll (listOf (elements [' ', '\n']))
    $ \val -> do
        export "SET_BUT_EMPTY" (T.pack val)
        v <- defaultingEnv "SET_BUT_EMPTY" "mydefault"
        v `shouldBe` "mydefault"

  it "provides the env var value if set" $ do
    export "DEFO_SET" "hithere"
    val <- defaultingEnv "DEFO_SET" "notgonnahappen"
    val `shouldBe` "hithere"
