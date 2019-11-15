module K8s.Resource
  ( ApiVersion(..)
  , Kind(..)
  , Resource(..)
  , ResourceSpec(..)
  , DeploymentSpec(..)
  , IngressSpec(..)
  , ServiceSpec(..)
  , Selector(..)
  , DeploymentConfig(..)
  , mkIngress
  , mkService
  , mkDeployment
  )
where

import           Data.Text                      ( Text
                                                , intercalate
                                                , splitOn
                                                )
import           GHC.Generics                   ( Generic )

import qualified Data.Map                      as Map

import           Data.Aeson.Types               ( genericToJSON )
import           Data.Yaml                      ( ToJSON(..) )

import           K8s.Container
import           K8s.Ingress
import           K8s.Metadata
import           K8s.Pod
import           K8s.Selector
import           K8s.Service
import           Values

import qualified K8s.JSON

data ApiVersion = ExtensionsV1Beta1 | AppsV1 | CoreV1
  deriving (Generic, Eq, Show)
instance ToJSON ApiVersion where
  toJSON ExtensionsV1Beta1 = "extensions/v1beta1"
  toJSON AppsV1            = "apps/v1"
  toJSON CoreV1            = "v1"

data Kind = Ingress | Deployment | Service
  deriving (Generic, Eq, Show)
instance ToJSON Kind

data ResourceSpec = IngressResource IngressSpec
                  | ServiceResource ServiceSpec
                  | DeploymentResource DeploymentSpec
  deriving (Generic, Eq, Show)
instance ToJSON ResourceSpec where
  toJSON = genericToJSON K8s.JSON.options

newtype IngressSpec = IngressSpec
                    { rules :: [IngressRule]
                    }
  deriving (Generic, Eq, Show)
instance ToJSON IngressSpec

data ServiceSpec = ServiceSpec
                    { _type :: !ServiceType
                    , ports :: ![ServicePort]
                    , selector :: !Selector
                    }
  deriving (Generic, Eq, Show)
instance ToJSON ServiceSpec where
  toJSON = genericToJSON K8s.JSON.options

data DeploymentSpec = DeploymentSpec
                    { replicas :: !Replicas
                    , selector :: !Selector
                    , template :: !PodTemplateSpec
                    }
  deriving (Generic, Eq, Show)
instance ToJSON DeploymentSpec

data Resource = Resource
  { apiVersion :: !ApiVersion
  , kind :: !Kind
  , metadata :: !Metadata
  , spec :: !ResourceSpec
  }
  deriving (Eq, Generic, Show)
instance ToJSON Resource

mkIngress :: Text -> Namespace -> [IngressRule] -> Resource
mkIngress app namespace rules = Resource
  { apiVersion = ExtensionsV1Beta1
  , kind       = Ingress
  , metadata   = Metadata
    { namespace   = Just namespace
    , name        = Just app
    , annotations = Map.singleton IngressClass "traefik"
    , labels      = mempty
    }
  , spec       = IngressResource $ IngressSpec {rules }
  }

mkService :: Text -> Namespace -> Text -> Resource
mkService app namespace servicePortName = Resource
  { apiVersion = CoreV1
  , kind       = Service
  , metadata   = Metadata
    { namespace   = Just namespace
    , name        = Just app
    , annotations = mempty
    , labels      = Map.singleton App app
    }
  , spec       = ServiceResource $ ServiceSpec
    { _type    = ClusterIP
    , ports    = [ ServicePort
                     { name       = servicePortName
                     , targetPort = servicePortName
                     , protocol   = TCP
                     , port       = Port 80
                     }
                 ]
    , selector = AppSelector app
    }
  }

data DeploymentConfig = DeploymentConfig
  { nodeAffinity :: !(Maybe NodeAffinity)
  , podAntiAffinity :: !(Maybe AntiAffinity)
  , app :: !Text
  , image :: !Text
  , namespace :: Namespace
  , port :: !Port
  , readinessProbe :: !(Maybe Probe)
  , livenessProbe :: !(Maybe Probe)
  , replicas :: !Replicas
  , resources :: !(Maybe ResourceRequirements)
  , portName :: !Text
  , terminationGracePeriodSeconds :: !(Maybe Duration)
  , tolerations :: ![Toleration]
  , volumes :: ![Volume]
  , volumeMounts :: ![VolumeMount]
  , preStop :: !(Maybe [Text])
  }

mkDeployment :: DeploymentConfig -> Resource
mkDeployment config@DeploymentConfig { app, namespace, portName, image, port, replicas, resources, nodeAffinity, podAntiAffinity, tolerations, readinessProbe, livenessProbe, terminationGracePeriodSeconds, volumes, preStop }
  = Resource
    { apiVersion = AppsV1
    , kind       = Deployment
    , metadata   = Metadata
      { namespace   = Just namespace
      , name        = Just app
      , annotations = Map.singleton JaegerInject "true"
      , labels      = Map.singleton App app
      }
    , spec       = DeploymentResource $ DeploymentSpec
      { replicas
      , selector = LabelSelector (Map.singleton App app)
      , template = PodTemplateSpec
        { metadata = Metadata
          { namespace   = Nothing
          , name        = Just app
          , annotations = mempty
          , labels      = Map.singleton App app
          }
        , spec     = PodSpec
          { affinity = Just Affinity {nodeAffinity , podAntiAffinity }
          , tolerations
          , terminationGracePeriodSeconds
          , volumes
          , containers                    = pure Container
            { livenessProbe  = livenessProbe
            , readinessProbe = readinessProbe
            , env            = buildEnv app namespace image
            , envFrom = [EnvFromSource {secretRef = SecretRef {name = app}}]
            , name           = app
            , image
            , ports = [ContainerPort {name = portName, containerPort = port}]
            , resources
            , volumeMounts   = K8s.Resource.volumeMounts config
            , lifecycle      = case preStop of
              (Just cmdArgs) ->
                Just
                  (Lifecycle
                    { preStop = Handler {exec = ExecAction {command = cmdArgs}}
                    }
                  )
              Nothing -> Nothing
            }
          }
        }
      }
    }


buildEnv :: Text -> Namespace -> Text -> [EnvVar]
buildEnv app (Namespace namespace) image = case splitOn ":" image of
  [] -> []
  (_ : version) ->
    [ EnvVar
      { name  = "SENTRY_RELEASE"
      , value = app <> "-" <> intercalate ":" version
      }
    , EnvVar
      { name  = "OPENTRACING_JAEGER_SERVICE_NAME"
      , value = app <> "." <> namespace
      }
    ]
