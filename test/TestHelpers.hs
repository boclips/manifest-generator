module TestHelpers
  ( TestPort(..)
  , TestReplicas(..)
  , asList
  )
where

import           GHC.Generics                   ( Generic )

import           Test.QuickCheck.Arbitrary      ( Arbitrary(..)
                                                , arbitrary
                                                )
import           Turtle

import qualified Control.Foldl                 as L

import           Values

asList :: MonadIO io => Shell a -> io [a]
asList script = fold script L.list

newtype TestPort = TestPort Port
  deriving (Generic, Show)
instance Arbitrary TestPort where
  arbitrary = TestPort . Port <$> arbitrary

newtype TestReplicas = TestReplicas Replicas
  deriving (Generic, Show)
instance Arbitrary TestReplicas where
  arbitrary = TestReplicas . Replicas <$> arbitrary
