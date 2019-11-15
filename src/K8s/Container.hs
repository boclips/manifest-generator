module K8s.Container
  ( Container(..)
  , ContainerPort(..)
  , EnvVar(..)
  , EnvFromSource(..)
  , ResourceRequirements(..)
  , ScaleDefinition(..)
  , Probe(..)
  , HTTPGetAction(..)
  , Duration(..)
  , SecretRef(..)
  , VolumeMount(..)
  , Lifecycle(..)
  , Handler(..)
  , ExecAction(..)
  )
where

import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

import           Data.Aeson.Types               ( genericToJSON )
import           Data.Yaml                      ( ToJSON
                                                , toJSON
                                                )

import           Values

import qualified K8s.JSON

data ContainerPort = ContainerPort
  { name :: !Text
  , containerPort :: !Port
  } deriving (Generic, Eq, Show)
instance ToJSON ContainerPort

data EnvVar = EnvVar
  { name :: !Text
  , value :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON EnvVar

newtype EnvFromSource = EnvFromSource
  { secretRef :: SecretRef
  } deriving (Generic, Eq, Show)
instance ToJSON EnvFromSource

newtype SecretRef = SecretRef
  { name:: Text
  } deriving (Generic, Eq, Show)
instance ToJSON SecretRef

data ScaleDefinition = ScaleDefinition
  { cpu :: !Text
  , memory :: !Text
  } deriving (Generic, Eq, Show)
instance ToJSON ScaleDefinition

data ResourceRequirements = ResourceRequirements
  { requests :: !ScaleDefinition
  , limits :: !ScaleDefinition
  } deriving (Generic, Eq, Show)
instance ToJSON ResourceRequirements

data HTTPGetAction = HTTPGetAction
  { path :: !Text
  , port :: !Port
  } deriving (Generic, Eq, Show)
instance ToJSON HTTPGetAction

data Probe = Probe
  { failureThreshold :: !Word
  , httpGet :: !HTTPGetAction
  , initialDelaySeconds :: !Duration
  , periodSeconds :: !Duration
  , successThreshold :: !Word
  , timeoutSeconds :: !Duration
  } deriving (Generic, Eq, Show)
instance ToJSON Probe

data VolumeMount = VolumeMount
  { mountPath :: !Text
  , name :: !Text
  , readOnly :: !Bool
  } deriving (Generic, Eq, Show)
instance ToJSON VolumeMount

data ExecAction = ExecAction
  { command :: ![Text]
  } deriving (Generic, Eq, Show)
instance ToJSON ExecAction

data Handler = Handler
  { exec :: !ExecAction
  } deriving (Generic, Eq, Show)
instance ToJSON Handler

data Lifecycle = Lifecycle
  { preStop :: !Handler
  } deriving (Generic, Eq, Show)
instance ToJSON Lifecycle

data Container = Container
  { name :: !Text
  , image :: !Text
  , ports :: ![ContainerPort]
  , env :: ![EnvVar]
  , envFrom :: ![EnvFromSource]
  , resources :: !(Maybe ResourceRequirements)
  , livenessProbe :: !(Maybe Probe)
  , readinessProbe :: !(Maybe Probe)
  , volumeMounts :: ![VolumeMount]
  , lifecycle :: !(Maybe Lifecycle)
  } deriving (Generic, Eq, Show)
instance ToJSON Container where
  toJSON = genericToJSON K8s.JSON.options
