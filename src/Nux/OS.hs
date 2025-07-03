{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nux.OS where

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

installFlake
  :: HasLogFunc env
  => FilePath          -- path to the flake directory
  -> String            -- root device (e.g., "/dev/sda1")
  -> String            -- hostname
  -> Bool              -- format root device
  -> Bool              -- force
  -> RIO env ()
installFlake flake rootDev hostname formatRoot isForce = do
  let hostDir = flake </> "nix/hosts" </> hostname
  rootDir <- if rootDev == ""
    then do
      logInfo "Using current root filesystem"
      return "/"
    else do
      logInfo $ fromString $ "Preparing root filesystem from device " <> rootDev
      when formatRoot $ do
        logInfo $ fromString $ "Formatting " <> rootDev
        mkfs isForce "btrfs" rootDev
      mountRoot rootDev
      return mnt
  generateHardwareConfig rootDir hostDir isForce
  let lockFile = flake </> "flake.lock"
  unlessM (doesFileExist lockFile) $ void $ nixFlake "lock" []
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

nixosInstallFlake :: FilePath -> FilePath -> String -> RIO env ()
nixosInstallFlake rootDir flake hostname = do
  cp flake $ rootDir </> "etc/nuxos"
  void $ exec "nixos-install"
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

mountRoot :: HasLogFunc env => String -> RIO env ()
mountRoot rootDev = do
  efiDev <- getEfiDevice
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
  whenM (mounted rootDev) $ do
    throwString $ "root device already mounted: " <> rootDev
  logInfo $ fromString $ "Mounting " <> rootDev <> " to " <> mnt
  mount rootDev mnt
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
      logWarn "Hardware config already exists, add --force to re-generate"
    else do
      logInfo $ fromString "Generating hardware configuration"
      hwConfig <-
        sudo "nixos-generate-config" $
          "--show-hardware-config":
          (if rootDir == "/" then [] else ["--root", rootDir])
      writeBinaryFile hwFile $ fromString hwConfig

withTempFlake :: (FilePath -> RIO env ()) -> RIO env ()
withTempFlake = withSystemTempDirectory "nuxos"
