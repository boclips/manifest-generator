module EnvParse
  ( defaultingEnv
  )
where

import           Data.Char
import           Data.Maybe
import           Data.Text                      ( all )
import           Prelude                 hiding ( all )
import           Turtle

defaultingEnv :: Text -> Text -> IO Text
defaultingEnv name defaultValue = do
  val <- need name
  pure $ case val of
    Nothing -> defaultValue
    Just v  -> if all isSpace v then defaultValue else v
