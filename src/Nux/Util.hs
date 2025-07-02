{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Util where

import           RIO
import           RIO.Char           (isSpace)
import           RIO.Directory
import qualified RIO.List           as L
import           System.Environment (lookupEnv)
import           System.Process     (readProcessWithExitCode, system)

split :: Eq a => a -> [a] -> [[a]]
split a as = case rest of
  []   -> [chunk]
  _:rs -> chunk : split a rs
  where (chunk, rest) = break (==a) as

nullOr :: Foldable t => t a -> t a -> t a
nullOr a b = if null a then b else a

emptyOrJust :: (Monoid a, Eq a) => a -> Maybe a
emptyOrJust a = if a == mempty then Nothing else Just a

zeroOrJust :: (Num a, Eq a) => a -> Maybe a
zeroOrJust a = if a == 0 then Nothing else Just a

zeroOrJust2 :: (Eq a, Num a) => a -> Maybe a -> Maybe a
zeroOrJust2 a b = if a == 0 then b else Just a

zeroOr :: (Eq a, Num a) => a -> a -> a
zeroOr a b = if a == 0 then b else a

splitOptions :: String -> [String]
splitOptions s
  = s
  & split ','
  & map trim
  & filter (/= "")
  & (`nullOr` ["defaults"])

exec :: MonadIO m => String -> [String] -> m String
exec cmd args = do
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess   -> return out
    ExitFailure _ -> throwString err

sudo :: String -> [String] -> RIO env String
sudo cmd args = exec "sudo" (cmd : args)

mount :: String -> String -> RIO env ()
mount dev mountPoint = void $ sudo "mount" [dev, mountPoint]

umount :: String -> RIO env ()
umount path = void $ sudo "umount" [path]

mounts :: RIO env [String]
mounts = exec "mount" [] <&> lines

mountpoint :: String -> RIO env Bool
mountpoint path =
  isRight <$> tryAny (exec "mountpoint" [path])

mounted :: String -> RIO env Bool
mounted path = any (L.isPrefixOf path) <$> mounts

getHostname :: MonadIO m => m String
getHostname = trim <$> exec "hostname" []

nixfmt :: String -> [String] -> RIO env ()
nixfmt f opts = void $ exec "nixfmt" (f : opts)

nix :: String -> [String] -> RIO env String
nix cmd args = exec "nix" (cmd : args)

nixrun :: [String] -> RIO env String
nixrun args = exec "nix" ("run" : args)

nixeval :: [String] -> RIO env String
nixeval args = exec "nix" ("eval" : args)

nixosPkgNames :: String -> RIO env String
nixosPkgNames osname =
  nix "eval" [ "--json"
             ,"./nuxos#nixosConfigurations." <> osname <> ".pkgs"
             , "--apply"
             , "builtins.attrNames"
             ]

nixosOptNames :: String -> RIO env String
nixosOptNames osname =
  nix "eval" [ "--json"
             ,"./nuxos#nixosConfigurations." <> osname <> ".options"
             , "--apply"
             , "builtins.attrNames"
             ]

nixosRebuild :: String -> [String] -> RIO env ()
nixosRebuild cmd args = void $ sudo "nixos-rebuild" (cmd : args)

nixosSwitch :: [String] -> RIO env ()
nixosSwitch = nixosRebuild "switch"

nixosOptionValue :: String -> String -> RIO env String
nixosOptionValue osname optname =
  nix "eval" ["--json", nixosOptionName osname optname]
  <&> trim

nixosOptionName :: String -> String -> String
nixosOptionName osname optname =
  "./nuxos#nixosConfigurations." <> osname <> ".options." <> optname <> ".value"

nixFlake :: String -> [String] -> RIO env String
nixFlake cmd args = nix "flake" (cmd:args)

getEfiDevice :: RIO env String
getEfiDevice =
  trim <$> sudo "blkid" [ "--list-one"
                        , "--match-token"
                        , "PARTLABEL=\"EFI system partition\""
                        , "--output"
                        , "device"
                        ]

mkdir :: String -> RIO env ()
mkdir = createDirectoryIfMissing True

cp :: FilePath -> FilePath -> RIO env ()
cp src tgt = void $ exec "cp" ["-r", src, tgt]

trimL :: String -> String
trimL = L.dropWhile isSpace

trimR :: String -> String
trimR = L.dropWhileEnd isSpace

trim :: String -> String
trim = trimL . trimR

mkfs :: Bool -> String -> String -> RIO env ()
mkfs isForce fs dev = void $ sudo cmd args
  where
    cmd = "mkfs." <> fs
    args = if isForce
      then ["-f", dev]
      else [dev]

exit :: Int -> RIO env ()
exit = liftIO . exitWith . code
  where
    code 0 = ExitSuccess
    code n = ExitFailure n

git :: String -> [String] -> RIO env String
git cmd args = exec "git" (cmd : args)

gitC :: String -> String -> [String] -> RIO env String
gitC repo cmd args = exec "git" (["-C", repo, cmd] ++ args)

gitConfig :: String -> [String] -> RIO env String
gitConfig name args = git "config" (name : args)

gitConfigGet :: String -> RIO env String
gitConfigGet name = gitConfig name []

gitConfigGetEmail :: RIO env String
gitConfigGetEmail = gitConfigGet "user.email"

getEnvDefault :: MonadIO m => String -> String -> m String
getEnvDefault var def = do
  liftIO $ lookupEnv var <&> fromMaybe def

edit :: MonadIO m => String -> String -> m ()
edit editor file = do
  defEditor <- getEnvDefault "EDITOR" "nano"
  let cmd = if editor == ""
            then defEditor
            else editor
  void $ liftIO $ system (cmd <> " " <> file)

isDirectoryEmpty :: FilePath -> RIO env Bool
isDirectoryEmpty dir = do
  isDir <- doesDirectoryExist dir
  unless isDir $ do
    throwString $ "not a directory: " <> dir
  contents <- listDirectory dir
  return $ null contents
