module RenderCommand
  ( main
  )
where

import           Prelude                 hiding ( FilePath )
import           Data.Text                      ( Text )

import           Data.ByteString                ( ByteString )
import           Turtle                         ( FilePath
                                                , Parser
                                                , optPath
                                                , options
                                                , optText
                                                )
import           Data.Yaml                      ( ToJSON
                                                , encode
                                                )

import qualified Data.ByteString.Char8         as BS

import           K8s.Resource
import           ReleaseConfig                  ( ManifestStyle(..)
                                                , Version(..)
                                                , manifestStyle
                                                )
import           Render                         ( EnvironmentResource
                                                , environmentName
                                                , resource
                                                , outputResources
                                                )

data Options = Options
  { path :: !FilePath
  , version :: !Text
  , environment :: !Text
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optPath "manifest" 'm' "Path to bo-manifest YAML"
    <*> optText "version" 'v' "Version number of app"
    <*> optText
          "environment"
          'e'
          "The name of the environment in the bo-manifest to render e.g staging"

concatYAMLDocs :: ToJSON a => [a] -> ByteString
concatYAMLDocs = foldl (\acc res -> acc <> "---\n" <> encode res) mempty

k8sResourcesForEnvironment :: Text -> [EnvironmentResource] -> [Resource]
k8sResourcesForEnvironment envName envResources =
  resource <$> filter (\res -> envName == environmentName res) envResources

main :: IO ()
main = do
  userOptions <- options
    "Render command - convert Bomanifests to K8s manifests"
    optionsParser
  style <- manifestStyle (path userOptions)

  case style of
    Boclips manifest -> BS.putStr $ concatYAMLDocs $ k8sResourcesForEnvironment
      (environment userOptions)
      (outputResources manifest (Version (version userOptions)))
