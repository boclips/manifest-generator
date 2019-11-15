module DeserialisationSpec
  ( spec
  )
where

import           Test.Hspec
import           Data.ByteString.Char8         as BS

import           Data.Yaml                      ( decodeEither' )

import           ReleaseConfig
import           Values

spec :: Spec
spec =
  it "deserialises a YAML config to its internal representation"
    $ let manifest = BS.unlines
            [ "app: great-app"
            , "port: 4000"
            , "healthPath: /foo/bar"
            , "scale: ExtraLarge"
            , "bootTime: Fast"
            , "preStop:"
            , "- sleep 5"
            , "environments:"
            , "  production:"
            , "    replicas: 999"
            , "    urls:"
            , "    - my.prod.url/withpath"
            , "  staging:"
            , "    replicas: 1"
            , "    urls:"
            , "    - my.staging.url/withpath"
            , "  testing:"
            , "    replicas: 0"
            , "    urls:"
            , "    - my.testing.url/withpath"
            ]
      in  case decodeEither' manifest of
            Left  _ -> fail "Exception when parsing!"
            Right m -> m `shouldBe` Bomanifest
              { app           = "great-app"
              , port          = Port 4000
              , healthPath    = Just "/foo/bar"
              , scale         = ExtraLarge
              , bootTime      = Just Fast
              , preStop       = Just ["sleep 5"]
              , mountedSecret = Nothing
              , environments  = AppEnvironments
                { production = Just AppEnv
                  { replicas = Replicas 999
                  , urls     = ["my.prod.url/withpath"]
                  }
                , staging    = Just AppEnv
                  { replicas = Replicas 1
                  , urls     = ["my.staging.url/withpath"]
                  }
                , testing    = Just AppEnv
                  { replicas = Replicas 0
                  , urls     = ["my.testing.url/withpath"]
                  }
                }
              }
