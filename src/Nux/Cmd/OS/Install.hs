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
import RIO.FilePath
import Nux.Options
import Nux.Util

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
  )

data InstallOptions = InstallOptions
  { installOptRootDev :: String
  , installOptFormat  :: Bool
  } deriving (Show, Eq)

runInstall :: InstallOptions -> RIO App ()
runInstall InstallOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  isForce <- view forceL
  let hostDir = flake </> "nix/hosts" </> hostname
  let hwFile = hostDir </> "hardware.nix"
  logInfo $ "Installing Nux system to " <> fromString installOptRootDev
  --efiMount <- nixosOptionValue "nux" "boot.loader.efi.efiSysMountPoint"
  let efiMount = "/boot/efi"
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
    mkfs isForce "btrfs" installOptRootDev
  whenM (mounted installOptRootDev) $ do
    logError $ fromString $ "Root device already mounted: " <> installOptRootDev <> ", please unmount before installation."
    exit 1
  logInfo $ fromString $ "Mounting " <> installOptRootDev <> " to " <> mnt
  mount installOptRootDev mnt
  void $ sudo "mkdir" ["-p", (mnt <> efiMount)]
  logInfo $ fromString $ "Mounting " <> efiDev <> " to " <> (mnt <> efiMount)
  mount efiDev (mnt <> efiMount)
  logInfo $ fromString $ "Generating hardware configuration"
  hwcfg <- sudo "nixos-generate-config"
    [ "--root", mnt
    , "--show-hardware-config"
    ]
  writeBinaryFile hwFile $ fromString hwcfg
  void $ sudo "mkdir" [mnt </> "etc"]
  logInfo $ fromString $ "Copying Nux system configuration to " <> mnt
  void $ sudo "cp" ["-r", flake, mnt </> "etc/nuxos"]
  logInfo $ fromString $ "Installing Nux system to " <> mnt
  void $ sudo "nixos-install" ["--flake", mnt </> "etc/nuxos#" <> hostname, "--root", mnt]
  logInfo $ fromString $ "Congradulations! Installation succeeded!"
  umount (mnt <> efiMount)
  umount mnt
  where
    mnt = "/mnt"
