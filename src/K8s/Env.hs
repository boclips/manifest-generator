module K8s.Env
  ( Env(..)
  )
where

import           GHC.Generics                   ( Generic )

import qualified Turtle

import           ReleaseConfig

data Env = Env
  { name      :: !Turtle.FilePath
  , namespace :: !Turtle.Text
  , domain    :: !Turtle.Text
  , version   :: !Turtle.Text
  , replicas  :: !Word
  , priority  :: !Priority
  }
  deriving (Show, Generic)
