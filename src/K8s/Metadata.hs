module K8s.Metadata
  ( AnnotationKey(..)
  , Annotations
  , LabelKey(..)
  , Labels
  , Metadata(..)
  , Namespace(..)
  )
where

import           Data.Text
import           Data.Yaml                      ( ToJSON )
import           GHC.Generics
import           Data.Aeson.Types               ( genericToJSON
                                                , ToJSONKey
                                                , toJSON
                                                , toJSONKey
                                                , toJSONKeyText
                                                )
import           Data.Map                       ( Map )

import qualified K8s.JSON

data AnnotationKey = IngressClass
                   | JaegerInject
  deriving (Show, Eq, Ord, Generic)

instance ToJSON AnnotationKey
instance ToJSONKey AnnotationKey where
  toJSONKey = toJSONKeyText textKey where
    textKey IngressClass = "kubernetes.io/ingress.class"
    textKey JaegerInject = "sidecar.jaegertracing.io/inject"

data LabelKey = App
  deriving (Show, Eq, Ord, Generic)

instance ToJSON LabelKey
instance ToJSONKey LabelKey where
  toJSONKey = toJSONKeyText textKey where
    textKey App = "app"

type Annotations = Map AnnotationKey Text
type Labels = Map LabelKey Text

newtype Namespace = Namespace Text
  deriving (Show, Eq, Generic)
instance ToJSON Namespace

data Metadata = Metadata
  { name :: !(Maybe Text)
  , namespace :: !(Maybe Namespace)
  , annotations :: !Annotations
  , labels :: !Labels
  }
  deriving (Generic, Eq, Show)
instance ToJSON Metadata where
  toJSON = genericToJSON K8s.JSON.options
