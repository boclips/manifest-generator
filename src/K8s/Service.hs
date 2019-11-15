module K8s.Service
  ( ServiceType(..)
  , ServicePort(..)
  , Protocol(..)
  )
where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Data.Yaml                      ( ToJSON
                                                , toJSON
                                                )

import           Values

data ServiceType = ClusterIP
  deriving (Generic, Eq, Show)
instance ToJSON ServiceType where
  toJSON ClusterIP = "ClusterIP"

data ServicePort = ServicePort
  { name :: !Text
  , targetPort :: !Text
  , protocol :: !Protocol
  , port :: !Port
  }
  deriving (Generic, Eq, Show)
instance ToJSON ServicePort
