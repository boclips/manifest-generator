module SerialisationSpec
  ( spec
  )
where

import           Data.ByteString.Char8         as BS
import           Data.Yaml
import           Test.Hspec

import qualified Data.Map.Strict               as Map

import           K8s.Container
import           K8s.Ingress
import           K8s.Metadata
import           K8s.Resource                   ( Kind(..)
                                                , ApiVersion(..)
                                                )
import           K8s.Selector
import           K8s.Service
import           Values

import qualified K8s.Pod                       as Pod
import qualified K8s.Resource                  as KR

spec :: Spec
spec = do
  it "serialises an ingress config to YAML"
    $          encode KR.Resource
                 { apiVersion = CoreV1
                 , kind       = Ingress
                 , metadata   = Metadata
                   { namespace   = Just (Namespace "my-namespace")
                   , name        = Just "my-lovely-app"
                   , annotations = Map.singleton IngressClass "traefik"
                   , labels      = mempty
                   }
                 , spec       = KR.IngressResource $ KR.IngressSpec
                   { rules = [ IngressRule
                                 { host = "example.com"
                                 , http = HttpIngressRuleValue
                                   { paths = [ HttpIngressPath
                                                 { backend = IngressBackend
                                                   { serviceName = "my-service-name"
                                                   , servicePort = "http"
                                                   }
                                                 , path    = "/v1"
                                                 }
                                             ]
                                   }
                                 }
                             ]
                   }
                 }
    `shouldBe` BS.unlines
                 [ "apiVersion: v1"
                 , "kind: Ingress"
                 , "spec:"
                 , "  rules:"
                 , "  - http:"
                 , "      paths:"
                 , "      - path: /v1"
                 , "        backend:"
                 , "          servicePort: http"
                 , "          serviceName: my-service-name"
                 , "    host: example.com"
                 , "metadata:"
                 , "  annotations:"
                 , "    kubernetes.io/ingress.class: traefik"
                 , "  namespace: my-namespace"
                 , "  name: my-lovely-app"
                 , "  labels: {}"
                 ]

  it "serialises a service config to YAML"
    $          encode KR.Resource
                 { apiVersion = CoreV1
                 , kind       = Service
                 , metadata   = Metadata
                   { namespace   = Just (Namespace "my-namespace")
                   , name        = Just "my-lovely-app"
                   , annotations = mempty
                   , labels      = Map.singleton App "my-lovely-app"
                   }
                 , spec       = KR.ServiceResource $ KR.ServiceSpec
                   { _type    = ClusterIP
                   , ports    = [ ServicePort
                                    { name       = "myportname"
                                    , targetPort = "mytarget"
                                    , protocol   = TCP
                                    , port       = Port 80
                                    }
                                ]
                   , selector = KR.AppSelector "my-lovely-app"
                   }
                 }
    `shouldBe` BS.unlines
                 [ "apiVersion: v1"
                 , "kind: Service"
                 , "spec:"
                 , "  selector:"
                 , "    app: my-lovely-app"
                 , "  ports:"
                 , "  - targetPort: mytarget"
                 , "    protocol: TCP"
                 , "    name: myportname"
                 , "    port: 80"
                 , "  type: ClusterIP"
                 , "metadata:"
                 , "  annotations: {}"
                 , "  namespace: my-namespace"
                 , "  name: my-lovely-app"
                 , "  labels:"
                 , "    app: my-lovely-app"
                 ]

  it "serialises a deployment config to YAML"
    $          encode KR.Resource
                 { apiVersion = AppsV1
                 , kind       = Deployment
                 , metadata   = Metadata
                   { namespace   = Just (Namespace "my-namespace")
                   , name        = Just "my-lovely-app"
                   , annotations = Map.singleton JaegerInject "true"
                   , labels      = Map.singleton App "my-lovely-app"
                   }
                 , spec       = KR.DeploymentResource $ KR.DeploymentSpec
                   { replicas = Replicas 3
                   , selector = KR.LabelSelector (Map.singleton App "my-lovely-app")
                   , template = Pod.PodTemplateSpec
                     { metadata = Metadata
                       { namespace   = Nothing
                       , name        = Nothing
                       , annotations = mempty
                       , labels      = Map.singleton App "my-lovely-app"
                       }
                     , spec     = Pod.PodSpec
                       { affinity                      = Just Pod.Affinity
                         { nodeAffinity    = Just Pod.NodeAffinity
                           { requiredDuringSchedulingIgnoredDuringExecution = Pod.NodeSelector
                             { nodeSelectorTerms = [ Pod.NodeSelectorTerm
                                                       { matchExpressions = [ Pod.NodeSelectorRequirement
                                                                                { key = "hi"
                                                                                , operator = "equals"
                                                                                , values = [ "there"
                                                                                           ]
                                                                                }
                                                                            ]
                                                       }
                                                   ]
                             }
                           }
                         , podAntiAffinity = Just Pod.AntiAffinity
                           { preferredDuringSchedulingIgnoredDuringExecution = [ Pod.WeightedPodAffinityTerm
                                                                                   { podAffinityTerm = Pod.AffinityTerm
                                                                                     { topologyKey = Pod.Zone
                                                                                     , labelSelector = AppSelector
                                                                                       "my-lovely-app"
                                                                                     }
                                                                                   , weight = 100
                                                                                   }
                                                                               ]
                           }
                         }
                       , tolerations                   = []
                       , terminationGracePeriodSeconds = Just (Duration 0)
                       , containers                    = pure Container
                         { livenessProbe  = Just Probe
                           { failureThreshold    = 1
                           , httpGet             = HTTPGetAction
                             { path = "/liveness/path"
                             , port = Port 1000
                             }
                           , initialDelaySeconds = Duration 2
                           , periodSeconds       = Duration 3
                           , successThreshold    = 4
                           , timeoutSeconds      = Duration 5
                           }
                         , readinessProbe = Just Probe
                           { failureThreshold    = 6
                           , httpGet             = HTTPGetAction
                             { path = "/readiness/path"
                             , port = Port 1000
                             }
                           , initialDelaySeconds = Duration 7
                           , periodSeconds       = Duration 8
                           , successThreshold    = 9
                           , timeoutSeconds      = Duration 10
                           }
                         , env = [EnvVar {name = "SOME_KEY", value = "SOME_VALUE"}]
                         , envFrom        = [ EnvFromSource
                                                { secretRef = SecretRef {name = "my-lovely-app"}
                                                }
                                            ]
                         , name           = "my-lovely-app"
                         , image = "eu.gcr.io/boclips-prod/boclips/my-lovely-app:1.2.3"
                         , ports          = [ ContainerPort
                                                { name          = "iloveports"
                                                , containerPort = Port 1234
                                                }
                                            ]
                         , resources      = Just
                           (ResourceRequirements
                             { requests = ScaleDefinition {cpu = "100m", memory = "10Gi"}
                             , limits = ScaleDefinition {cpu = "1", memory = "100Gi"}
                             }
                           )
                         , volumeMounts   = [ VolumeMount
                                                { mountPath = "/foo/bar"
                                                , name      = "foobar"
                                                , readOnly  = True
                                                }
                                            ]
                         , lifecycle      = Nothing
                         }
                       , volumes                       = [ Pod.Volume
                                                             { name = "hi-i-am-a-volume"
                                                             , secret = Pod.SecretVolumeSource
                                                               { secretName = "a-secret-name"
                                                               }
                                                             }
                                                         ]
                       }
                     }
                   }
                 }
    `shouldBe` BS.unlines
                 [ "apiVersion: apps/v1"
                 , "kind: Deployment"
                 , "spec:"
                 , "  selector:"
                 , "    matchLabels:"
                 , "      app: my-lovely-app"
                 , "  template:"
                 , "    spec:"
                 , "      affinity:"
                 , "        nodeAffinity:"
                 , "          requiredDuringSchedulingIgnoredDuringExecution:"
                 , "            nodeSelectorTerms:"
                 , "            - matchExpressions:"
                 , "              - operator: equals"
                 , "                values:"
                 , "                - there"
                 , "                key: hi"
                 , "        podAntiAffinity:"
                 , "          preferredDuringSchedulingIgnoredDuringExecution:"
                 , "          - podAffinityTerm:"
                 , "              labelSelector:"
                 , "                app: my-lovely-app"
                 , "              topologyKey: failure-domain.beta.kubernetes.io/zone"
                 , "            weight: 100"
                 , "      terminationGracePeriodSeconds: 0"
                 , "      tolerations: []"
                 , "      containers:"
                 , "      - livenessProbe:"
                 , "          successThreshold: 4"
                 , "          failureThreshold: 1"
                 , "          timeoutSeconds: 5"
                 , "          initialDelaySeconds: 2"
                 , "          httpGet:"
                 , "            path: /liveness/path"
                 , "            port: 1000"
                 , "          periodSeconds: 3"
                 , "        image: eu.gcr.io/boclips-prod/boclips/my-lovely-app:1.2.3"
                 , "        readinessProbe:"
                 , "          successThreshold: 9"
                 , "          failureThreshold: 6"
                 , "          timeoutSeconds: 10"
                 , "          initialDelaySeconds: 7"
                 , "          httpGet:"
                 , "            path: /readiness/path"
                 , "            port: 1000"
                 , "          periodSeconds: 8"
                 , "        env:"
                 , "        - value: SOME_VALUE"
                 , "          name: SOME_KEY"
                 , "        volumeMounts:"
                 , "        - name: foobar"
                 , "          mountPath: /foo/bar"
                 , "          readOnly: true"
                 , "        resources:"
                 , "          limits:"
                 , "            memory: 100Gi"
                 , "            cpu: '1'"
                 , "          requests:"
                 , "            memory: 10Gi"
                 , "            cpu: 100m"
                 , "        name: my-lovely-app"
                 , "        ports:"
                 , "        - name: iloveports"
                 , "          containerPort: 1234"
                 , "        envFrom:"
                 , "        - secretRef:"
                 , "            name: my-lovely-app"
                 , "      volumes:"
                 , "      - secret:"
                 , "          secretName: a-secret-name"
                 , "        name: hi-i-am-a-volume"
                 , "    metadata:"
                 , "      annotations: {}"
                 , "      labels:"
                 , "        app: my-lovely-app"
                 , "  replicas: 3"
                 , "metadata:"
                 , "  annotations:"
                 , "    sidecar.jaegertracing.io/inject: 'true'"
                 , "  namespace: my-namespace"
                 , "  name: my-lovely-app"
                 , "  labels:"
                 , "    app: my-lovely-app"
                 ]
