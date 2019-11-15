module K8s.JSON
  ( options
  )
where

import           Data.Aeson.Types               ( Options(..)
                                                , SumEncoding(..)
                                                , defaultOptions
                                                )

options :: Options
options = defaultOptions { sumEncoding        = UntaggedValue
                         , fieldLabelModifier = stripUnderscorePrefix
                         , omitNothingFields  = True
                         }
 where
  stripUnderscorePrefix ('_' : label) = label
  stripUnderscorePrefix label         = label
