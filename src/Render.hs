module Render
  ( outputResources
  , EnvironmentResource(..)
  )
where

import           Prelude                 hiding ( FilePath )
import           Data.Text                      ( Text )

import           Turtle                         ( fromText )

import           Conversions                    ( EnvironmentResource(..) )
import           K8s.Resource
import           K8s.Metadata                   ( Namespace(..) )
import           ReleaseConfig                 as RC
import           Values

import qualified Conversions                   as Convert

outputResources :: Bomanifest -> Version -> [EnvironmentResource]
outputResources Bomanifest { app, port, scale, bootTime, healthPath, environments, mountedSecret, preStop } version
  = case environments of
    AppEnvironments { testing, staging, production } ->
      []
        <> maybeEnv
             (\replicas urls ->
               ingressesFor "testing" urls
                 <> [ mkServiceEnvResource "testing"
                    , mkDeploymentEnvResource "testing"
                                              replicas
                                              Convert.preProduction
                    ]
             )
             testing
        <> maybeEnv
             (\replicas urls ->
               ingressesFor "staging" urls
                 <> [ mkServiceEnvResource "staging"
                    , mkDeploymentEnvResource "staging"
                                              replicas
                                              Convert.preProduction
                    ]
             )
             staging
        <> maybeEnv
             (\replicas urls ->
               ingressesFor "production" urls
                 <> [ mkServiceEnvResource "production"
                    , mkDeploymentEnvResource "production" replicas id
                    ]
             )
             production
 where
  maybeEnv
    :: (Replicas -> [Text] -> [EnvironmentResource])
    -> Maybe AppEnv
    -> [EnvironmentResource]
  maybeEnv f appEnv = maybe [] (\env -> f (RC.replicas env) (urls env)) appEnv

  ingressesFor :: Text -> [Text] -> [EnvironmentResource]
  ingressesFor environmentName urls = if length urls == 0
    then []
    else [mkIngressEnvResource environmentName urls]

  mkIngressEnvResource :: Text -> [Text] -> EnvironmentResource
  mkIngressEnvResource environmentName urls = EnvironmentResource
    { environmentName
    , path = Convert.mkPath app "ingress.yaml" (fromText environmentName)
    , resource = mkIngress app
                           (Namespace environmentName)
                           (Convert.urlsToRules app urls)
    }

  mkServiceEnvResource :: Text -> EnvironmentResource
  mkServiceEnvResource environmentName = EnvironmentResource
    { environmentName
    , path = Convert.mkPath app "service.yaml" (fromText environmentName)
    , resource = mkService app
                           (Namespace environmentName)
                           Convert.servicePortName
    }

  mkDeploymentEnvResource
    :: Text
    -> Replicas
    -> (DeploymentConfig -> DeploymentConfig)
    -> EnvironmentResource
  mkDeploymentEnvResource environmentName replicas f = EnvironmentResource
    { environmentName
    , path = Convert.mkPath app "deployment.yaml" (fromText environmentName)
    , resource = mkDeployment $ f DeploymentConfig
      { app
      , port
      , replicas
      , preStop
      , nodeAffinity                  = Nothing
      , podAntiAffinity = Just (Convert.podsPreferDifferentZones app)
      , image                         = Convert.imageName app version
      , namespace                     = Namespace environmentName
      , readinessProbe = Convert.mkReadinessProbe port healthPath bootTime
      , livenessProbe                 = Convert.mkLivenessProbe port healthPath
      , resources                     = Convert.tshirtSizesToResources scale
      , portName                      = Convert.servicePortName
      , terminationGracePeriodSeconds = Convert.gracePeriod
      , tolerations                   = []
      , volumes = Convert.mountedSecretToVolumes mountedSecret
      , volumeMounts = Convert.mountedSecretToVolumeMounts mountedSecret
      }
    }
