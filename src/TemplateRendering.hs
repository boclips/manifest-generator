module TemplateRendering
  ( renderTemplates
  )
where

import           Data.Functor
import           Data.Text                      ( Text )
import           Prelude                 hiding ( FilePath )
import           Turtle                         ( (</>)
                                                , (<|>)
                                                , FilePath
                                                , Line
                                                , MonadIO
                                                , Pattern
                                                , Shell
                                                , filename
                                                , findtree
                                                , input
                                                , ls
                                                , output
                                                , sed
                                                , sh
                                                , suffix
                                                , text
                                                )

import qualified Data.Text                     as T

import           K8s.Env
import           ReleaseConfig

renderTemplates :: MonadIO io => FilePath -> FilePath -> Env -> io ()
renderTemplates releasePath inputManifestsDir env = sh $ do
  template <- findtree (suffix ".yaml") (ls inputManifestsDir)
  output (releasePath </> filename template)
    $         "DOMAIN"
    `becomes` T.takeWhile (/= '\n') (domain env)
    $         "REPLICAS"
    `becomes` (showText . K8s.Env.replicas) env
    $         "VERSION"
    `becomes` T.stripEnd (version (env :: Env))
    $         "TOLERATE_NONCRITICAL_EXCEPT_PRODUCTION"
    `becomes` tolerateNoncritical env
    $         input template
 where
  showText :: Show a => a -> Text
  showText = T.stripEnd . T.pack . show

becomes :: Text -> Text -> Shell Line -> Shell Line
becomes needle replacement = sed (envVarPattern needle $> replacement)

envVarPattern :: Text -> Pattern Text
envVarPattern s = ("${" <> text s <> "}") <|> ("$" <> text s)

tolerateNoncritical :: Env -> Text
tolerateNoncritical env = case priority env of
  Critical -> mempty
  Noncritical
    -> "tolerations: [{key: priority, operator: Equal, value: noncritical, effect: NoSchedule}]"
