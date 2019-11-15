module ReleaseCommand
  ( main
  )
where

import           Prelude                 hiding ( FilePath )

import           Turtle                         ( FilePath
                                                , Parser
                                                , optPath
                                                , options
                                                )

import           Release

optionsParser :: Parser FilePath
optionsParser = optPath "manifest" 'm' "Path to bo-manifest YAML"

main :: IO ()
main = do
  manifestPath <- options "Release command - render manifests as git commits"
                          optionsParser
  release manifestPath
