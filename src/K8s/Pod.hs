module K8s.Pod
  ( PodTemplateSpec(..)
  , PodSpec(..)
  , Affinity(..)
  , AffinityTerm(..)
  , AntiAffinity(..)
  , Toleration(..)
  , TopologyKey(..)
  , NodeAffinity(..)
  , NodeSelector(..)
  , NodeSelectorTerm(..)
  , NodeSelectorRequirement(..)
  , WeightedPodAffinityTerm(..)
  , Volume(..)
  , SecretVolumeSource(..)
  )
where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Data.List.NonEmpty             ( NonEmpty )

import           Data.Aeson.Types               ( ToJSON
                                                , genericToJSON
                                                , toJSON
                                                )

import           K8s.Container
import           K8s.Metadata
import           K8s.Selector

import qualified K8s.JSON

data NodeSelectorRequirement = NodeSelectorRequirement { key :: !Text
                                                       , operator :: !Text
                                                       , values :: ![Text]
                                                       }
  deriving (Generic, Eq, Show)
instance ToJSON NodeSelectorRequirement

newtype NodeSelectorTerm = NodeSelectorTerm { matchExpressions :: [NodeSelectorRequirement] }
  deriving (Generic, Eq, Show)
instance ToJSON NodeSelectorTerm

newtype NodeSelector = NodeSelector { nodeSelectorTerms :: [NodeSelectorTerm] }
  deriving (Generic, Eq, Show)
instance ToJSON NodeSelector

newtype NodeAffinity = NodeAffinity { requiredDuringSchedulingIgnoredDuringExecution :: NodeSelector }
  deriving (Generic, Eq, Show)
instance ToJSON NodeAffinity

data Affinity = Affinity { nodeAffinity :: !(Maybe NodeAffinity)
                         , podAntiAffinity :: !(Maybe AntiAffinity)
                         }
  deriving (Generic, Eq, Show)
instance ToJSON Affinity

data AntiAffinity = AntiAffinity
  { preferredDuringSchedulingIgnoredDuringExecution :: ![WeightedPodAffinityTerm]
  }
  deriving (Generic, Eq, Show)
instance ToJSON AntiAffinity

data WeightedPodAffinityTerm = WeightedPodAffinityTerm
  { podAffinityTerm :: !AffinityTerm
  , weight :: !Int
  }
  deriving (Generic, Eq, Show)
instance ToJSON WeightedPodAffinityTerm

data TopologyKey = Zone
  deriving (Generic, Eq, Show)
instance ToJSON TopologyKey where
  toJSON Zone = "failure-domain.beta.kubernetes.io/zone"

data AffinityTerm = AffinityTerm
  { labelSelector :: !Selector
  , topologyKey :: !TopologyKey
  }
  deriving (Generic, Eq, Show)
instance ToJSON AffinityTerm

data Toleration = Toleration
  { key :: !Text
  , operator :: !Text
  , value :: !Text
  , effect :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON Toleration

data SecretVolumeSource = SecretVolumeSource
  { secretName :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON SecretVolumeSource

data Volume = Volume
  { name :: !Text
  , secret :: !SecretVolumeSource
  } deriving (Generic, Eq, Show)
instance ToJSON Volume

data PodSpec = PodSpec
  { affinity :: !(Maybe Affinity)
  , containers :: !(NonEmpty Container)
  , terminationGracePeriodSeconds :: !(Maybe Duration)
  , tolerations :: ![Toleration]
  , volumes :: ![Volume]
  } deriving (Generic, Eq, Show)
instance ToJSON PodSpec where
  toJSON = genericToJSON K8s.JSON.options

data PodTemplateSpec = PodTemplateSpec
  { metadata :: !Metadata
  , spec :: !PodSpec
  } deriving (Generic, Eq, Show)
instance ToJSON PodTemplateSpec where
  toJSON = genericToJSON K8s.JSON.options
