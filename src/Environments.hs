module Environments where

import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )

import qualified Data.Text                     as Text

import           Turtle                         ( Text )

import           K8s.Env
import           ReleaseConfig

mkEnvironments :: Vars -> [Env]
mkEnvironments source =
  [ Env
    { name      = "testing"
    , namespace = "testing"
    , domain    = "testing-boclips.com"
    , version   = version (source :: Vars)
    , replicas  = parseReplicas (testingReplicas source)
    , priority  = Noncritical
    }
  , Env
    { name      = "staging"
    , namespace = "staging"
    , domain    = "staging-boclips.com"
    , version   = version (source :: Vars)
    , replicas  = parseReplicas (stagingReplicas source)
    , priority  = Noncritical
    }
  , Env
    { name      = "production"
    , namespace = "production"
    , domain    = "boclips.com"
    , version   = version (source :: Vars)
    , replicas  = parseReplicas (productionReplicas source)
    , priority  = Critical
    }
  ]

parseReplicas :: Text -> Word
parseReplicas = fromMaybe 1 . readMaybe . Text.unpack
