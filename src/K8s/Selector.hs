module K8s.Selector
  ( Selector(..)
  )
where

import           GHC.Generics                   ( Generic )
import           Data.Text                      ( Text )
import           Data.Yaml                      ( ToJSON
                                                , toJSON
                                                , object
                                                , (.=)
                                                )

import           K8s.Metadata

data Selector = LabelSelector Labels
              | AppSelector Text
  deriving (Generic, Eq, Show)
instance ToJSON Selector where
  toJSON (LabelSelector labels) =
    object [ "matchLabels" .= labels ]
  toJSON (AppSelector app) =
    object [ "app" .= app ]
