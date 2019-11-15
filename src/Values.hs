module Values
  ( Port(..)
  , Protocol(..)
  , Replicas(..)
  , Duration(..)
  )
where

import           GHC.Generics                   ( Generic )

import           Data.Yaml                      ( FromJSON(..)
                                                , ToJSON(..)
                                                )

newtype Port = Port Word
  deriving (Show, Eq, Generic)
instance FromJSON Port
instance ToJSON Port

newtype Replicas = Replicas Word
  deriving (Show, Eq, Generic)
instance FromJSON Replicas
instance ToJSON Replicas

data Protocol = TCP
  deriving (Generic, Eq, Show)
instance ToJSON Protocol where
  toJSON TCP = "TCP"

newtype Duration = Duration Word
  deriving (Generic, Eq, Show)
instance ToJSON Duration
