module Conversions
  ( EnvironmentResource(..)
  , gracePeriod
  , imageName
  , mkPath
  , mkLivenessProbe
  , mkReadinessProbe
  , mountedSecretToVolumes
  , mountedSecretToVolumeMounts
  , podsPreferDifferentZones
  , preProduction
  , servicePortName
  , tolerateNoncritical
  , tshirtSizesToResources
  , urlsToRules
  )
where

import           Prelude                 hiding ( FilePath )
import           Data.Maybe                     ( catMaybes )

import qualified Data.Map                      as Map
import qualified Data.Text                     as Text

import           Data.Text                      ( Text )
import           Turtle                         ( FilePath
                                                , (</>)
                                                , fromText
                                                )
import           K8s.Container                  ( HTTPGetAction(..)
                                                , Probe(..)
                                                , ResourceRequirements(..)
                                                , ScaleDefinition(..)
                                                , VolumeMount(..)
                                                )
import           K8s.Ingress
import           K8s.Metadata
import           K8s.Pod
import           K8s.Resource                   ( DeploymentConfig(..)
                                                , Resource
                                                )
import           K8s.Selector                   ( Selector(..) )
import           ReleaseConfig
import           Values

data EnvironmentResource = EnvironmentResource
  { path :: !FilePath
  , resource :: !Resource
  , environmentName :: !Text
  }

preProduction :: DeploymentConfig -> DeploymentConfig
preProduction cfg = cfg { nodeAffinity = Just requireNoncritical
                        , tolerations  = [tolerateNoncritical]
                        }

mkPath :: Text -> FilePath -> FilePath -> FilePath
mkPath appName resourceFilename envName =
  envName </> fromText appName </> resourceFilename

mkReadinessProbe :: Port -> Maybe Text -> Maybe BootTime -> Maybe Probe
mkReadinessProbe port maybeHealthPath maybeBootTime = do
  healthPath <- maybeHealthPath
  bootTime   <- maybeBootTime

  Just $ case bootTime of
    Fast -> Probe
      { failureThreshold    = 2
      , httpGet             = HTTPGetAction {path = healthPath, port }
      , initialDelaySeconds = Duration 10
      , periodSeconds       = Duration 10
      , successThreshold    = 1
      , timeoutSeconds      = Duration 5
      }
    Slow -> Probe
      { failureThreshold    = 6
      , httpGet             = HTTPGetAction {path = healthPath, port }
      , initialDelaySeconds = Duration 20
      , periodSeconds       = Duration 10
      , successThreshold    = 1
      , timeoutSeconds      = Duration 5
      }

mkLivenessProbe :: Port -> Maybe Text -> Maybe Probe
mkLivenessProbe port maybeHealthPath = do
  healthPath <- maybeHealthPath

  Just $ Probe
    { failureThreshold    = 2
    , httpGet             = HTTPGetAction {path = healthPath, port }
    , initialDelaySeconds = Duration 80
    , periodSeconds       = Duration 10
    , successThreshold    = 1
    , timeoutSeconds      = Duration 5
    }

imageName :: Text -> Version -> Text
imageName app (Version ver) =
  "eu.gcr.io/boclips-prod/boclips/" <> app <> ":" <> ver

gracePeriod :: Maybe Duration
gracePeriod = Just (Duration 30)

tshirtSizesToResources :: Scale -> Maybe ResourceRequirements
tshirtSizesToResources scale = Just $ case scale of
  Tiny -> ResourceRequirements
    { requests = ScaleDefinition {cpu = "1m", memory = "100Mi"}
    , limits   = ScaleDefinition {cpu = "50m", memory = "200Mi"}
    }
  Small -> ResourceRequirements
    { requests = ScaleDefinition {cpu = "100m", memory = "80Mi"}
    , limits   = ScaleDefinition {cpu = "100m", memory = "80Mi"}
    }
  Medium -> ResourceRequirements
    { requests = ScaleDefinition {cpu = "400m", memory = "600Mi"}
    , limits   = ScaleDefinition {cpu = "400m", memory = "600Mi"}
    }
  Large -> ResourceRequirements
    { requests = ScaleDefinition {cpu = "1000m", memory = "500Mi"}
    , limits   = ScaleDefinition {cpu = "1000m", memory = "2Gi"}
    }
  ExtraLarge -> ResourceRequirements
    { requests = ScaleDefinition {cpu = "2500m", memory = "8Gi"}
    , limits   = ScaleDefinition {cpu = "3500m", memory = "16Gi"}
    }

servicePortName :: Text
servicePortName = "http"

urlsToRules :: Text -> [Text] -> [IngressRule]
urlsToRules serviceName urls = catMaybes $ urlToRule <$> urls
 where
  urlToRule :: Text -> Maybe IngressRule
  urlToRule url = case Text.splitOn "/" url of
    []     -> Nothing
    [host] -> Just (mkIngressRule host mempty)
    host : pathPart : pathParts ->
      Just (mkIngressRule host (Text.intercalate "/" (pathPart : pathParts)))

  mkIngressRule :: Text -> Text -> IngressRule
  mkIngressRule host path = IngressRule
    { host
    , http = HttpIngressRuleValue
      { paths = [ HttpIngressPath
                    { path    = "/" <> path
                    , backend = IngressBackend
                      { serviceName
                      , servicePort = servicePortName
                      }
                    }
                ]
      }
    }

requireNoncritical :: NodeAffinity
requireNoncritical = NodeAffinity
  { requiredDuringSchedulingIgnoredDuringExecution = NodeSelector
    { nodeSelectorTerms = [ NodeSelectorTerm
                              { matchExpressions = [ NodeSelectorRequirement
                                                       { key = "priority"
                                                       , operator = "In"
                                                       , values = [ "noncritical"
                                                                  ]
                                                       }
                                                   ]
                              }
                          ]
    }
  }

tolerateNoncritical :: Toleration
tolerateNoncritical = Toleration
  { key      = "priority"
  , operator = "Equal"
  , value    = "noncritical"
  , effect   = "NoSchedule"
  }

podsPreferDifferentZones :: Text -> AntiAffinity
podsPreferDifferentZones app = AntiAffinity
  { preferredDuringSchedulingIgnoredDuringExecution = [ WeightedPodAffinityTerm
                                                          { podAffinityTerm = AffinityTerm
                                                            { topologyKey = Zone
                                                            , labelSelector = LabelSelector
                                                              (Map.singleton
                                                                App
                                                                app
                                                              )
                                                            }
                                                          , weight = 100
                                                          }
                                                      ]
  }

mountedSecretToVolumes :: Maybe Text -> [Volume]
mountedSecretToVolumes maybeConfig = case maybeConfig of
  Nothing -> []
  Just name ->
    [Volume {name = name, secret = SecretVolumeSource {secretName = name}}]

mountedSecretToVolumeMounts :: Maybe Text -> [VolumeMount]
mountedSecretToVolumeMounts maybeConfig = case maybeConfig of
  Nothing -> []
  Just name ->
    [ VolumeMount
        { mountPath = "/secrets/" <> name
        , name      = name
        , readOnly  = True
        }
    ]
