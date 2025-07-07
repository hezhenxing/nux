{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.OS where

import           Nux.Host
import           Nux.User
import           Nux.Util
import           RIO
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List      as L

initFlake :: FilePath -> String -> RIO env ()
initFlake flake url = do
  let flakeFile = flake </> "flake.nix"
  writeBinaryFile flakeFile $ fromString $ L.unlines
    [ "{"
    , "  inputs.nuxos.url = \"" <> url <> "\";"
    , "  outputs = inputs: inputs.nuxos ./. {"
    , "    inherit inputs;"
    , "  };"
    , "}"
    ]

createFlake
 :: HasLogFunc env
 => FilePath     -- flake
 -> String       -- url
 -> String       -- hostname
 -> Host
 -> String       -- username
 -> User
 -> Bool         -- force
 -> RIO env ()
createFlake flake url hostname host username user isForce = do
  let hostDir = hostDirPath flake hostname
  let userDir = userDirPath flake username
  createDirectoryIfMissing True flake
  isEmpty <- isDirectoryEmpty flake
  unless isEmpty $ do
    if isForce
      then do
        logWarn "Directory is not empty, Forcing overwrite existing files..."
      else do
        logError "The target directory is not empty."
        throwString $ "directory not empty: " <> flake
  logInfo $ fromString $ "Initializing NuxOS configurations in " <> flake
  initFlake flake url
  logInfo $ fromString $ "Adding host " <> hostDir
  logDebug $ fromString $ L.unlines
    [ "name:        " <>       hostname
    , "system:      " <>       hostSystem host
    , "packages:    " <> show (hostAutos host)
    ]
  addFlakeHost flake hostname host
  logInfo $ fromString $ "Adding user " <> userDir
  logDebug $ fromString $ L.unlines
    [ "name:        " <>       username
    , "uid:         " <> show (userUid user)
    , "gid:         " <> show (userGid user)
    , "description: " <>       userDescription user
    , "email:       " <>       userEmail user
    , "packages:    " <> show (userAutos user)
    ]
  addFlakeUser flake username user

installFlake
  :: HasLogFunc env
  => FilePath          -- path to the flake directory
  -> FilePath          -- root directory (e.g., "/" or "/mnt")
  -> String            -- hostname
  -> Bool              -- symLink
  -> Bool              -- force
  -> RIO env ()
installFlake flake rootDir hostname symLink isForce = do
  let lockFile = flake </> "flake.lock"
  unlessM (doesFileExist lockFile) $ void $ nixFlake "lock" []
  exists <- doesPathExist $ rootDir </> "etc/nuxos"
  when (exists && not isForce) $ do
    throwString "target root filesystem already has NuxOS installed, use --force if you want to overwrite it"
  let nuxosPath = rootDir </> "etc/nuxos"
  void $ sudo "mkdir" ["-p", rootDir </> "etc"]
  void $ sudo "rm" ["-rf", nuxosPath]
  if symLink
    then do
      logInfo $ fromString $ "Creating symbolic link of NuxOS configuation at " <> nuxosPath
      void $ sudo "ln" ["-s", flake, nuxosPath]
    else do
      logInfo $ fromString $ "Copying NuxOS configuration to " <> nuxosPath
      void $ sudo "cp" ["-r", flake, nuxosPath]
  logInfo $ fromString $ "Installing NuxOS to " <> rootDir
  if rootDir == "/"
    then do
      nixosSwitchFlake flake hostname
    else do
      nixosInstallFlake mnt flake hostname
      umountRoot
  logInfo "Congradulations! Installation succeeded!"
  where
    mnt = "/mnt"

prepareRoot :: HasLogFunc env => String -> Bool -> Bool -> RIO env FilePath
prepareRoot rootDev formatRoot isForce = do
  if rootDev == ""
    then do
      logInfo "Using current root filesystem"
      return "/"
    else do
      logInfo $ fromString $ "Preparing root filesystem from device " <> rootDev
      checkMountPoint
      when formatRoot $ do
        logInfo $ fromString $ "Formatting " <> rootDev
        mkfs isForce "btrfs" rootDev
      mountRoot rootDev
      return mnt
  where
    mnt = "/mn"

nixosInstallFlake :: FilePath -> FilePath -> String -> RIO env ()
nixosInstallFlake rootDir flake hostname = do
  void $ sudo "mkdir" ["-p", rootDir </> "etc"]
  void $ sudo "cp" ["-r", flake, rootDir </> "etc/nuxos"]
  void $ sudo "nixos-install"
    [ "--flake"
    , flake </> "#" <> hostname
    , "--root"
    , rootDir]

nixosSwitchFlake :: FilePath -> String -> RIO env ()
nixosSwitchFlake flake hostname =
  void $ exec "nh"
    [ "os"
    , "switch"
    , flake
    , "--hostname"
    , hostname
    ]

checkMountPoint :: HasLogFunc env => RIO env ()
checkMountPoint = do
  mountpoint efiMount >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint in use: " <> (mnt <> efiMount) <> ", unmounting..."
      umount efiMount
  mountpoint mnt >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint in use: " <> mnt <> ", unmounting..."
      umount mnt
  where
    mnt = "/mnt"
    efiMount = mnt <> "/boot/efi"

mountRoot :: HasLogFunc env => String -> RIO env ()
mountRoot rootDev = do
  efiDev <- getEfiDevice
  whenM (mounted rootDev) $ do
    throwString $ "root device already mounted: " <> rootDev
  logInfo $ fromString $ "Mounting " <> rootDev <> " to " <> mnt
  mount rootDev mnt
  void $ sudo "mkdir" ["-p", efiMount]
  logInfo $ fromString $ "Mounting " <> efiDev <> " to " <> efiMount
  mount efiDev efiMount
  where
    mnt = "/mnt"
    efiMount = mnt <> "/boot/efi"

umountRoot :: RIO env ()
umountRoot = do
  umount efiMount
  umount mnt
  where
    mnt = "/mnt"
    efiMount = mnt <> "/boot/efi"

generateHardwareConfig
  :: HasLogFunc env
  => FilePath
  -> FilePath
  -> Bool
  -> RIO env ()
generateHardwareConfig rootDir hostDir isForce = do
  let hwFile = hostDir </> "hardware.nix"
  exists <- doesFileExist hwFile
  if exists && not isForce
    then do
      logWarn "Hardware config already exists, add --generate to re-generate"
    else do
      logInfo $ fromString "Generating hardware configuration"
      hwConfig <-
        sudo "nixos-generate-config" $
          "--show-hardware-config":
          (if rootDir == "/" then [] else ["--root", rootDir])
      writeBinaryFile hwFile $ fromString hwConfig

withTempFlake :: (FilePath -> RIO env ()) -> RIO env ()
withTempFlake = withSystemTempDirectory "nuxos"
