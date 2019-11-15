module K8s.Ingress
  ( IngressRule(..)
  , HttpIngressRuleValue(..)
  , HttpIngressPath(..)
  , IngressBackend(..)
  )
where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Data.Yaml                      ( ToJSON(..) )

data IngressBackend = IngressBackend
  { serviceName :: !Text
  , servicePort :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON IngressBackend

data HttpIngressPath = HttpIngressPath
  { backend :: !IngressBackend
  , path    :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON HttpIngressPath

newtype HttpIngressRuleValue = HttpIngressRuleValue
  { paths :: [HttpIngressPath]
  } deriving (Generic, Eq, Show)
instance ToJSON HttpIngressRuleValue

data IngressRule = IngressRule
  { host :: !Text
  , http :: !HttpIngressRuleValue
  } deriving (Generic, Eq, Show)
instance ToJSON IngressRule
