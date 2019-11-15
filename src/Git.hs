module Git
  ( add
  , getGit
  , getExitCode
  , initialise
  , rm
  , commit
  , config
  , configGet
  , clone
  , lsFiles
  , noChanges
  , showRefs
  , showCurrentAuthorTimestamp
  , status
  , GitContext
  )
where

import           Prelude                 hiding ( FilePath )

import           Turtle                  hiding ( rm )

type GitContext io = [Text] -> io ()

add :: [FilePath] -> [Text]
add paths = "add" : pathsToTexts paths

rm :: [FilePath] -> [Text]
rm paths = "rm" : "-rf" : "--ignore-unmatch" : "--quiet" : pathsToTexts paths

commit :: Text -> [Text]
commit msg =
  "-c"
    : "user.email=engineering@boclips.com"
    : "-c"
    : "user.name='Release Robot'"
    : "commit"
    : "--quiet"
    : "--message"
    : [msg]

config :: Text -> Text -> [Text]
config key value = "config" : key : [value]

configGet :: Text -> [Text]
configGet key = "config" : "--get" : [key]

status :: [Text]
status = ["status"]

noChanges :: [Text]
noChanges = "diff-index" : "--quiet" : ["HEAD"]

lsFiles :: [Text]
lsFiles = ["ls-files"]

showRefs :: [Text] -> [Text]
showRefs refs = "show" : refs

showCurrentAuthorTimestamp :: [Text]
showCurrentAuthorTimestamp =
  "show" : "HEAD" : "--no-patch" : ["--pretty=format:%at"]

getGit :: FilePath -> [Text] -> Shell Line
getGit contextDir args =
  inproc "git" ("-C" : format fp contextDir : args) empty

getExitCode :: MonadIO io => FilePath -> [Text] -> io ExitCode
getExitCode contextDir args =
  proc "git" ("-C" : format fp contextDir : args) empty

initialise :: MonadIO io => FilePath -> io (GitContext io)
initialise dstPath = do
  procs "git" ("init" : "--quiet" : [format fp dstPath]) empty
  pure $ ioContext dstPath

clone :: MonadIO io => Text -> FilePath -> io (GitContext io)
clone src dstPath = do
  procs "git" ("clone" : "--quiet" : src : [format fp dstPath]) empty
  pure $ ioContext dstPath

ioContext :: MonadIO io => FilePath -> GitContext io
ioContext contextDir args =
  procs "git" ("-C" : format fp contextDir : args) empty

pathsToTexts :: [FilePath] -> [Text]
pathsToTexts = map (format fp)
