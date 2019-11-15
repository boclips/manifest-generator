module ReleaseConfig
  ( ManifestStyle(..)
  , manifestStyle
  , Vars(..)
  , AppEnvironments(..)
  , AppEnv(..)
  , Bomanifest(..)
  , Priority(..)
  , Scale(..)
  , BootTime(..)
  , Version(..)
  )
where

import           Prelude                 hiding ( FilePath )
import           Data.Text                     as Text
import           GHC.Generics                   ( Generic )

import           Data.Yaml                      ( FromJSON(..)
                                                , decodeFileEither
                                                )
import           Turtle                         ( FilePath
                                                , die
                                                , format
                                                , fp
                                                )

import qualified Turtle

import           Values

data Priority = Critical | Noncritical
  deriving (Show)

data ManifestStyle = Boclips Bomanifest | K8s

manifestStyle :: FilePath -> IO ManifestStyle
manifestStyle path = do
  boManifestPresent <- Turtle.testfile path
  if boManifestPresent
    then do
      contents <- decodeFileEither (unpack (format fp path))
      case contents of
        Left  e          -> die (Text.pack (show e))
        Right bomanifest -> pure (Boclips bomanifest)
    else pure K8s

data Vars = Vars
  { testingReplicas    :: !Text
  , stagingReplicas    :: !Text
  , productionReplicas :: !Text
  , version            :: !Text
  }
  deriving (Show)

data AppEnvironments = AppEnvironments
  { production :: !(Maybe AppEnv)
  , staging :: !(Maybe AppEnv)
  , testing :: !(Maybe AppEnv)
  }
  deriving (Show, Eq, Generic)
instance FromJSON AppEnvironments

data AppEnv = AppEnv
  { replicas :: !Replicas
  , urls :: ![Text]
  }
  deriving (Show, Eq, Generic)
instance FromJSON AppEnv

newtype Version = Version Text
  deriving (Show, Eq, Generic)
instance FromJSON Version

data Scale = Tiny | Small | Medium | Large | ExtraLarge
  deriving (Show, Eq, Generic)
instance FromJSON Scale

data BootTime = Fast | Slow
  deriving (Show, Eq, Generic)
instance FromJSON BootTime

data Bomanifest = Bomanifest
  { app :: !Text
  , port :: !Port
  , healthPath :: !(Maybe Text)
  , mountedSecret :: !(Maybe Text)
  , scale :: !Scale
  , bootTime :: !(Maybe BootTime)
  , preStop :: !(Maybe [Text])
  , environments :: !AppEnvironments
  }
  deriving (Show, Eq, Generic)
instance FromJSON Bomanifest
