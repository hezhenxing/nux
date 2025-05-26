{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.OS.Install
  ( installCmd,
  )
where

import RIO
import RIO.File
import Nux.Options
import Nux.Util
import System.Process (system)

installCmd :: Command (RIO App ())
installCmd = addCommand
  "install"
  "Install Nux system"
  runInstall
  (InstallOptions <$> strArgument ( metavar "ROOTDEV"
                                 <> help "Root device to install Nux system"
                                  )
                  <*> switch ( long "format"
                                 <> help "Format the root device before installation"
                                 )
                  <*> switch ( long "force"
                                 <> help "Force formatting the root device"
                                 )
  )

data InstallOptions = InstallOptions
  { installOptRootDev :: String
  , installOptFormat  :: Bool
  , installOptForce   :: Bool
  } deriving (Show, Eq)

runInstall :: InstallOptions -> RIO App ()
runInstall InstallOptions{..} = do
  logInfo $ "Installing Nux system to " <> fromString installOptRootDev
  efiMount <- nixosOptionValue "nux" "boot.loader.efi.efiSysMountPoint"
  efiDev <- getEfiDevice
  mountpoint (mnt <> efiMount) >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint already exists: " <> (mnt <> efiMount) <> ", unmounting..."
      umount (mnt <> efiMount)
  mountpoint mnt >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint already exists: " <> mnt <> ", unmounting..."
      umount mnt
  when installOptFormat $ do
    logInfo $ fromString $ "Formatting " <> installOptRootDev
    mkfs installOptForce "btrfs" installOptRootDev
  whenM (mounted installOptRootDev) $ do
    logError $ fromString $ "Root device already mounted: " <> installOptRootDev <> ", please unmount before installation."
    exit 1
  logInfo $ fromString $ "Mounting " <> installOptRootDev <> " to " <> mnt
  mount installOptRootDev mnt
  sudo "mkdir" ["-p", (mnt <> efiMount)]
  logInfo $ fromString $ "Mounting " <> efiDev <> " to " <> (mnt <> efiMount)
  mount efiDev (mnt <> efiMount)
  sudo "mkdir" [mnt <> "/etc"]
  logInfo $ fromString $ "Copying Nux system to " <> mnt
  sudo "cp" ["-r", "./nuxos", mnt <> "/etc"]
  logInfo $ fromString $ "Generating hardware configuration"
  hwcfg <- sudo "nixos-generate-config"
    [ "--root", mnt
    , "--show-hardware-config"
    ]
  sudo "mkdir" [mnt <> "/tmp"]
  sudo "chmod" ["0777", mnt <> "/tmp"]
  writeBinaryFile (mnt <> "/tmp" <> "/hardware.nix") $ fromString hwcfg
  sudo "mv" [mnt <> "/tmp/hardware.nix", mnt <> "/etc/nuxos/nix/nixos/nux/hardware.nix"]
  logInfo $ fromString $ "Installing Nux system to " <> mnt
  sudo "nixos-install" ["--flake", mnt <> "/etc/nuxos#nux", "--root", mnt]
  logInfo $ fromString $ "Congradulations! Installation succeeded!"
  umount (mnt <> efiMount)
  umount mnt
  where
    mnt = "/mnt"
