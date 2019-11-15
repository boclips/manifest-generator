module RenderSpec
  ( spec
  )
where

import           Data.Text                      ( unpack )
import           Prelude                 hiding ( FilePath )

import qualified Data.List.NonEmpty            as NonEmpty

import           Turtle                         ( FilePath
                                                , format
                                                , fp
                                                )
import           Test.Hspec
import           Test.QuickCheck                ( property )

import qualified Data.Map.Strict               as Map

import           K8s.Container
import           K8s.Ingress
import           K8s.Resource                   ( ApiVersion(..)
                                                , Kind(..)
                                                , ResourceSpec(..)
                                                )
import           K8s.Metadata
import           K8s.Service
import           ReleaseConfig
import           Render
import           TestHelpers
import           Values

import qualified K8s.Metadata                  as Metadata
import qualified K8s.Resource                  as KR
import qualified K8s.Pod                       as Pod
import qualified K8s.Selector                  as Selector

fromEnvironmentResource :: EnvironmentResource -> (FilePath, KR.Resource)
fromEnvironmentResource EnvironmentResource { path, resource } =
  (path, resource)

lookupEnv :: FilePath -> [EnvironmentResource] -> IO KR.Resource
lookupEnv path envResources =
  case lookup path (fromEnvironmentResource <$> envResources) of
    Nothing  -> fail (unpack ("Couldn't find fixture for " <> format fp path))
    Just res -> return res

spec :: Spec
spec = do
  let
    renderedResources = outputResources
      Bomanifest
        { app           = "bo-selecta"
        , port          = Port 4000
        , scale         = Large
        , bootTime      = Just Fast
        , preStop       = Nothing
        , healthPath    = Just "/my/healthendpoint"
        , mountedSecret = Just "google-application-credentials"
        , environments  = AppEnvironments
          { production = Just AppEnv {replicas = Replicas 999, urls = []}
          , staging    = Just AppEnv
            { replicas = Replicas 2
            , urls     = ["staging.url"]
            }
          , testing    = Just AppEnv
            { replicas = Replicas 1
            , urls     = ["testing.url/testing/api"]
            }
          }
        }
      (Version "1.2.3")

    expectedAppLabels = Map.singleton App "bo-selecta"

  describe "ingresses" $ do
    it "outputs k8s ingress for host-only app URL"
      $ lookupEnv "staging/bo-selecta/ingress.yaml" renderedResources
      `shouldReturn` KR.Resource
                       { apiVersion = ExtensionsV1Beta1
                       , kind       = Ingress
                       , metadata   = Metadata
                         { namespace   = Just (Namespace "staging")
                         , name        = Just "bo-selecta"
                         , annotations = Map.singleton Metadata.IngressClass
                                                       "traefik"
                         , labels      = mempty
                         }
                       , spec       = KR.IngressResource $ KR.IngressSpec
                         { rules = [ IngressRule
                                       { http = HttpIngressRuleValue
                                         { paths = [ HttpIngressPath
                                                       { path = "/"
                                                       , backend = IngressBackend
                                                         { serviceName = "bo-selecta"
                                                         , servicePort = "http"
                                                         }
                                                       }
                                                   ]
                                         }
                                       , host = "staging.url"
                                       }
                                   ]
                         }
                       }

    it "splits host-and-path app URLs into parts" $ do
      r <- lookupEnv "testing/bo-selecta/ingress.yaml" renderedResources
      KR.spec r `shouldBe` IngressResource KR.IngressSpec
        { rules = [ IngressRule
                      { http = HttpIngressRuleValue
                        { paths = [ HttpIngressPath
                                      { path    = "/testing/api"
                                      , backend = IngressBackend
                                        { serviceName = "bo-selecta"
                                        , servicePort = "http"
                                        }
                                      }
                                  ]
                        }
                      , host = "testing.url"
                      }
                  ]
        }

    it "doesn't create an ingress when there's no URL specified" $ do
      lookup "production/bo-selecta/ingress.yaml"
             (fromEnvironmentResource <$> renderedResources)
        `shouldBe` Nothing

  describe "services"
    $ it "outputs k8s service"
    $ lookupEnv "production/bo-selecta/service.yaml" renderedResources
    `shouldReturn` KR.Resource
                     { apiVersion = CoreV1
                     , kind       = Service
                     , metadata   = Metadata
                       { namespace            = Just (Namespace "production")
                       , Metadata.name        = Just "bo-selecta"
                       , Metadata.annotations = mempty
                       , Metadata.labels      = expectedAppLabels
                       }
                     , spec       = KR.ServiceResource $ KR.ServiceSpec
                       { _type    = ClusterIP
                       , ports    = [ ServicePort
                                        { name       = "http"
                                        , targetPort = "http"
                                        , protocol   = TCP
                                        , port       = Port 80
                                        }
                                    ]
                       , selector = KR.AppSelector "bo-selecta"
                       }
                     }

  describe "deployments" $ do
    it "sets tolerations for testing" $ do
      res <- lookupEnv "testing/bo-selecta/deployment.yaml" renderedResources
      podTolerations res
        `shouldBe` [ Pod.Toleration
                       { key      = "priority"
                       , operator = "Equal"
                       , value    = "noncritical"
                       , effect   = "NoSchedule"
                       }
                   ]

    it "doesn't set tolerations for production" $ do
      res <- lookupEnv "production/bo-selecta/deployment.yaml" renderedResources
      podTolerations res `shouldBe` []

    it "sets pod anti-affinity in production" $ do
      res <- lookupEnv "production/bo-selecta/deployment.yaml" renderedResources
      antiAffinity res `shouldBe` Just Pod.AntiAffinity
        { preferredDuringSchedulingIgnoredDuringExecution = [ Pod.WeightedPodAffinityTerm
                                                                { weight = 100
                                                                , podAffinityTerm = Pod.AffinityTerm
                                                                  { topologyKey = Pod.Zone
                                                                  , labelSelector = Selector.LabelSelector
                                                                    (Map.singleton
                                                                      App
                                                                      "bo-selecta"
                                                                    )
                                                                  }
                                                                }
                                                            ]
        }

    it "creates a volume out of a secret" $ do
      res <- lookupEnv "production/bo-selecta/deployment.yaml" renderedResources
      volumes res
        `shouldBe` [ Pod.Volume
                       { name   = "google-application-credentials"
                       , secret = Pod.SecretVolumeSource
                         { secretName = "google-application-credentials"
                         }
                       }
                   ]

    it "mounts the volume created by a secret" $ do
      res <- lookupEnv "production/bo-selecta/deployment.yaml" renderedResources
      RenderSpec.volumeMounts res
        `shouldBe` [ K8s.Container.VolumeMount
                       { mountPath = "/secrets/google-application-credentials"
                       , name      = "google-application-credentials"
                       , readOnly  = True
                       }
                   ]

    it "outputs a staging k8s deployment"
      $ property
      $ \(TestPort port) (TestReplicas instances) -> do
          let
            input = Bomanifest
              { app           = "bo-selecta"
              , port
              , healthPath    = Just "/my/healthendpoint"
              , scale         = Medium
              , bootTime      = Just Fast
              , preStop       = Just ["sh", "-c", "sleep 5"]
              , mountedSecret = Nothing
              , environments  = AppEnvironments
                { production = Just AppEnv
                  { replicas = instances
                  , urls     = ["prod.url"]
                  }
                , staging    = Just AppEnv
                  { replicas = instances
                  , urls     = ["staging.url"]
                  }
                , testing    = Just AppEnv
                  { replicas = instances
                  , urls     = ["testing.url"]
                  }
                }
              }

          lookupEnv "staging/bo-selecta/deployment.yaml"
                    (outputResources input (Version "1.2.3"))
            `shouldReturn` KR.Resource
                             { apiVersion = AppsV1
                             , kind       = Deployment
                             , metadata   = Metadata
                               { name        = Just "bo-selecta"
                               , namespace   = Just (Namespace "staging")
                               , annotations = Map.singleton JaegerInject "true"
                               , labels      = expectedAppLabels
                               }
                             , spec = KR.DeploymentResource $ KR.DeploymentSpec
                               { replicas = instances
                               , selector = KR.LabelSelector expectedAppLabels
                               , template = Pod.PodTemplateSpec
                                 { metadata = Metadata
                                   { name        = Just "bo-selecta"
                                   , namespace   = Nothing
                                   , annotations = mempty
                                   , labels      = expectedAppLabels
                                   }
                                 , spec     = Pod.PodSpec
                                   { containers = pure Container
                                     { livenessProbe  = Just Probe
                                       { httpGet             = HTTPGetAction
                                         { path = "/my/healthendpoint"
                                         , port = port
                                         }
                                       , failureThreshold    = 2
                                       , initialDelaySeconds = Duration 80
                                       , periodSeconds       = Duration 10
                                       , successThreshold    = 1
                                       , timeoutSeconds      = Duration 5
                                       }
                                     , readinessProbe = Just Probe
                                       { httpGet             = HTTPGetAction
                                         { path = "/my/healthendpoint"
                                         , port = port
                                         }
                                       , failureThreshold    = 2
                                       , initialDelaySeconds = Duration 10
                                       , periodSeconds       = Duration 10
                                       , successThreshold    = 1
                                       , timeoutSeconds      = Duration 5
                                       }
                                     , resources = (Just ResourceRequirements
                                                     { requests = ScaleDefinition
                                                       { cpu    = "400m"
                                                       , memory = "600Mi"
                                                       }
                                                     , limits = ScaleDefinition
                                                       { cpu    = "400m"
                                                       , memory = "600Mi"
                                                       }
                                                     }
                                                   )
                                     , name           = "bo-selecta"
                                     , image = "eu.gcr.io/boclips-prod/boclips/bo-selecta:1.2.3"
                                     , ports          = [ ContainerPort
                                                            { name = "http"
                                                            , containerPort = port
                                                            }
                                                        ]
                                     , envFrom        = [ EnvFromSource
                                                            { secretRef = SecretRef
                                                              { name = "bo-selecta"
                                                              }
                                                            }
                                                        ]
                                     , env = [ EnvVar "SENTRY_RELEASE"
                                                      "bo-selecta-1.2.3"
                                             , EnvVar
                                               "OPENTRACING_JAEGER_SERVICE_NAME"
                                               "bo-selecta.staging"
                                             ]
                                     , volumeMounts   = []
                                     , lifecycle      = Just Lifecycle
                                       { preStop = Handler
                                         { exec = ExecAction
                                           { command = ["sh", "-c", "sleep 5"]
                                           }
                                         }
                                       }
                                     }
                                   , Pod.volumes = []
                                   , terminationGracePeriodSeconds = Just
                                     (Duration 30)
                                   , affinity = Just Pod.Affinity
                                     { nodeAffinity    = Just Pod.NodeAffinity
                                       { requiredDuringSchedulingIgnoredDuringExecution = Pod.NodeSelector
                                         { nodeSelectorTerms = [ Pod.NodeSelectorTerm
                                                                   { matchExpressions = [ Pod.NodeSelectorRequirement
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
                                     , podAntiAffinity = Just Pod.AntiAffinity
                                       { preferredDuringSchedulingIgnoredDuringExecution = [ Pod.WeightedPodAffinityTerm
                                                                                               { weight = 100
                                                                                               , podAffinityTerm = Pod.AffinityTerm
                                                                                                 { topologyKey = Pod.Zone
                                                                                                 , labelSelector = Selector.LabelSelector
                                                                                                   (Map.singleton
                                                                                                     App
                                                                                                     "bo-selecta"
                                                                                                   )
                                                                                                 }
                                                                                               }
                                                                                           ]
                                       }
                                     }
                                   , tolerations = [ Pod.Toleration
                                                       { key      = "priority"
                                                       , operator = "Equal"
                                                       , value = "noncritical"
                                                       , effect   = "NoSchedule"
                                                       }
                                                   ]
                                   }
                                 }
                               }
                             }

    it "doesn't produce a probe when health path is not set" $ do
      let
        input = Bomanifest
          { app           = "bo-selecta"
          , port          = Port 1000
          , healthPath    = Nothing
          , scale         = Medium
          , bootTime      = Nothing
          , preStop       = Nothing
          , mountedSecret = Nothing
          , environments  = AppEnvironments
            { production = Just AppEnv
              { replicas = Replicas 1
              , urls     = ["prod.url"]
              }
            , staging    = Just AppEnv
              { replicas = Replicas 1
              , urls     = ["staging.url"]
              }
            , testing    = Just AppEnv
              { replicas = Replicas 1
              , urls     = ["testing.url"]
              }
            }
          }
      resource <- lookupEnv "production/bo-selecta/deployment.yaml"
                            (outputResources input (Version "2.3.4"))
      ( livenessProbe (firstContainer resource)
        , readinessProbe (firstContainer resource)
        )
        `shouldBe` (Nothing, Nothing)

    it "doesn't produce manifests for environments that aren't configured" $ do
      let rendered = outputResources
            Bomanifest
              { app           = "missing-some"
              , port          = Port 4000
              , scale         = Large
              , bootTime      = Nothing
              , preStop       = Nothing
              , healthPath    = Nothing
              , mountedSecret = Nothing
              , environments  = AppEnvironments
                { production = Just AppEnv
                  { replicas = Replicas 1
                  , urls     = ["prod.url/api"]
                  }
                , staging    = Nothing
                , testing    = Just AppEnv
                  { replicas = Replicas 1
                  , urls     = ["testing.url"]
                  }
                }
              }
            (Version "1.2.3")
      lookup "staging" (fromEnvironmentResource <$> rendered) `shouldBe` Nothing

    describe "adjustments" $ do
      describe "for production"
        $ it "doesn't set node affinity, so pods land on critical nodes"
        $ do
            res <- lookupEnv "production/bo-selecta/deployment.yaml"
                             renderedResources
            nodeAffinity res `shouldBe` Nothing

nodeAffinity :: KR.Resource -> Maybe Pod.NodeAffinity
nodeAffinity res = do
  aff <- podAffinity res
  Pod.nodeAffinity aff

antiAffinity :: KR.Resource -> Maybe Pod.AntiAffinity
antiAffinity res = do
  aff <- podAffinity res
  Pod.podAntiAffinity aff

podAffinity :: KR.Resource -> Maybe Pod.Affinity
podAffinity res = Pod.affinity (Pod.spec (KR.template sp))
  where sp = requireDeployment res

podTolerations :: KR.Resource -> [Pod.Toleration]
podTolerations res = Pod.tolerations (Pod.spec (KR.template sp))
  where sp = requireDeployment res

firstContainer :: KR.Resource -> Container
firstContainer res = NonEmpty.head (Pod.containers (Pod.spec (KR.template sp)))
  where sp = requireDeployment res

volumes :: KR.Resource -> [Pod.Volume]
volumes res = Pod.volumes (Pod.spec (KR.template sp))
  where sp = requireDeployment res

volumeMounts :: KR.Resource -> [K8s.Container.VolumeMount]
volumeMounts res = K8s.Container.volumeMounts (firstContainer res)

requireDeployment :: KR.Resource -> KR.DeploymentSpec
requireDeployment res = case KR.spec res of
  DeploymentResource sp -> sp
  _                     -> error "Didn't pass a deployment resource"
