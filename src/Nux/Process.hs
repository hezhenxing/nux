{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Process where

import           Data.Aeson
import           Nux.Util
import           Prelude             (read, showChar, showString)
import           RIO
import qualified RIO.ByteString.Lazy as BL
import           RIO.Char
import qualified RIO.List            as L
import           RIO.Process
import qualified RIO.Text            as T
import qualified System.Exit         as Exit

data Proc = Proc
  { procCommand :: String
  , procArgs    :: [String]
  } deriving (Show)

emptyProc :: Proc
emptyProc = Proc
  ""
  []


cmd :: String -> Proc
cmd c = emptyProc { procCommand = c }

arg :: String -> Proc -> Proc
arg a p = p { procArgs = procArgs p ++ [a] }

args :: [String] -> Proc -> Proc
args as p = p { procArgs = procArgs p ++ as}

env :: String -> String -> Proc -> Proc
env k v p = p { procCommand = "env"
              , procArgs = if procCommand p == "env"
                  then k <> "=" <> v : procArgs p
                  else k <> "=" <> v : procCommand p : procArgs p
              }

envs :: [(String, String)] -> Proc -> Proc
envs kvs p = p { procCommand = "env"
              , procArgs = if procCommand p == "env"
                  then map (\(k, v) -> k <> "=" <> v) kvs ++ procArgs p
                  else map (\(k, v) -> k <> "=" <> v) kvs ++ procCommand p : procArgs p
               }

shell :: Proc -> Proc
shell p@Proc{..} = p { procCommand = "/bin/sh"
            , procArgs = [ "-c", cmdline]
            }
  where
    cmdline = showCommandForShell procCommand procArgs

-- Copied from showCommandForUser, add '~' to goodChar
showCommandForShell :: String -> [String] -> String
showCommandForShell s as = unwords (map translate (s : as))
  where
    translate "" = "''"
    translate str
      | all goodChar str = str
      | otherwise        = '\'' : foldr escape "'" str
    escape '\'' = showString "'\\''"
    escape c    = showChar c
    goodChar c  = isAlphaNum c || c `elem` "-_.,/~"

sudo :: Proc -> Proc
sudo p = p { procCommand = "sudo"
           , procArgs = procCommand p: procArgs p
           }

run :: (HasProcessContext env, HasLogFunc env) => Proc -> RIO env ()
run Proc{..} =
  proc procCommand procArgs runProcess_

runExitCode :: (HasProcessContext env, HasLogFunc env) => Proc -> RIO env ExitCode
runExitCode Proc{..} =
  proc procCommand procArgs runProcess

readStdout :: (HasProcessContext env, HasLogFunc env) => Proc -> RIO env BL.ByteString
readStdout Proc{..} =
  proc procCommand procArgs readProcessStdout_

readStdoutString :: (HasProcessContext env, HasLogFunc env) => Proc -> RIO env String
readStdoutString p =
  readStdout p <&> T.unpack . decodeUtf8Lenient . BL.toStrict

readStdoutLines :: (HasProcessContext env, HasLogFunc env) => Proc -> RIO env [String]
readStdoutLines p =
  readStdout p <&> lines . T.unpack . decodeUtf8Lenient . BL.toStrict

flakeUpdate
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env ()
flakeUpdate flakeDir
  = cmd "nix"
  & arg "flake"
  & arg "update"
  & arg "--flake"
  & arg flakeDir
  & run

flakeSwitch
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env ()
flakeSwitch flakeDir hostname
  = cmd "nh"
  & arg "os"
  & arg "switch"
  & arg flakeDir
  & arg "--hostname"
  & arg hostname
  & run

flakeTest
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env ()
flakeTest flakeDir hostname
  = cmd "nh"
  & arg "os"
  & arg "test"
  & arg flakeDir
  & arg "--hostname"
  & arg hostname
  & run

flakeInstall
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> FilePath -> String -> RIO env ()
flakeInstall rootDir flakeDir hostname
  = cmd "nixos-install"
  & arg "--no-root-passwd"
  & arg "--root"
  & arg rootDir
  & arg "--flake"
  & arg (flakeDir <> "#" <> hostname)
  & sudo
  & run

die :: MonadIO m => String -> m a
die = liftIO . Exit.die

exit :: Int -> RIO env ()
exit = liftIO . exitWith . code
  where
    code 0 = ExitSuccess
    code n = ExitFailure n

mkdir
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env ()
mkdir path = cmd "mkdir" & arg "-p" & arg path & run

cp
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> FilePath -> RIO env ()
cp src tgt = cmd "cp" & arg "-r" & arg src & arg tgt & run

mkfs
  :: (HasProcessContext env, HasLogFunc env)
  => Bool -> String -> String -> RIO env ()
mkfs isForce fsType dev
  = cmd "mkfs"
  & arg "-t"
  & arg fsType
  & args ["-f" | isForce]
  & arg dev
  & sudo
  & run

mount
  :: (HasProcessContext env, HasLogFunc env)
  => String -> FilePath -> RIO env ()
mount dev path
  = cmd "mount"
  & arg dev
  & arg path
  & sudo
  & run

umount
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env ()
umount path = cmd "umount" & arg path & sudo & run

mounts
  :: (HasProcessContext env, HasLogFunc env)
  => RIO env [String]
mounts = cmd "mount" & readStdout
  <&> lines . T.unpack . decodeUtf8Lenient . BL.toStrict

mountpoint
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env Bool
mountpoint path
  = cmd "mountpoint"
  & arg path
  & run
  & tryAny
  <&> isRight

mounted
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env Bool
mounted path = mounts <&> any (L.isPrefixOf path)

getEfiDevice
  :: (HasProcessContext env, HasLogFunc env) => RIO env String
getEfiDevice
  = cmd "blkid"
  & arg "--list-one"
  & arg "--match-token"
  & arg "PARTLABEL=\"EFI system partition\""
  & arg "--output"
  & arg "device"
  & sudo
  & readStdoutString
  <&> trim

git :: Proc
git = cmd "git"

gitConfig :: Proc
gitConfig = git & arg "config"

gitConfigGetEmail
  :: (HasProcessContext env, HasLogFunc env)
  => RIO env String
gitConfigGetEmail
  = gitConfig
  & arg "user.email"
  & readStdoutString

nixEvalJsonProc :: Proc
nixEvalJsonProc = cmd "nix" & arg "eval" & arg "--json"

nixEvalAttr
  :: (HasProcessContext env, HasLogFunc env, FromJSON a)
  => FilePath -> String -> RIO env a
nixEvalAttr flakeDir attrName = do
  content <- nixEvalJsonProc
    & arg (flakeDir <> "#" <> attrName)
    & readStdout
  case eitherDecode content of
    Left err -> throwString err
    Right  r -> return r

nixEvalAttrNames
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env [String]
nixEvalAttrNames flakeDir attrName = do
  content <- nixEvalJsonProc
    & arg (flakeDir <> "#" <> attrName)
    & arg "--apply"
    & arg "builtins.attrNames"
    & readStdout
  case eitherDecode content of
    Left err -> throwString err
    Right  r -> return r

blockDevSize
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env Int
blockDevSize dev
  = cmd "blockdev"
  & arg "--getsize64"
  & arg dev
  & sudo
  & readStdoutString
  <&> read . trim
