module ParsingSpec
  ( spec
  )
where

import           Data.Text.Arbitrary            ( )
import           Test.Hspec
import           Test.QuickCheck         hiding ( output )

import           Environments

spec :: Spec
spec = it "can parse replicas" $ property $ total . parseReplicas
