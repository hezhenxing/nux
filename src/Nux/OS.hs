{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Nux.OS where

import           Nux.Util
import           RIO
import           RIO.Directory (createDirectoryIfMissing)
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
  let efiMount = "/boot/efi"
  efiDev <- getEfiDevice
  mountpoint (mnt <> efiMount) >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint in use: " <> (mnt <> efiMount) <> ", unmounting..."
      umount (mnt <> efiMount)
  mountpoint mnt >>= \case
    False -> return ()
    True -> do
      logWarn $ fromString $ "Mountpoint in use: " <> mnt <> ", unmounting..."
      umount mnt
  when formatRoot $ do
    logInfo $ fromString $ "Formatting " <> rootDev
    mkfs isForce "btrfs" rootDev
  whenM (mounted rootDev) $ do
    throwString $ "root device already mounted: " <> rootDev
  logInfo $ fromString $ "Mounting " <> rootDev <> " to " <> mnt
  mount rootDev mnt
  logInfo $ fromString $ "Mounting " <> efiDev <> " to " <> (mnt <> efiMount)
  mount efiDev (mnt <> efiMount)
  logInfo $ fromString "Generating hardware configuration"
  void $ exec "nixos-generate-config"
    [ "--root", mnt
    , "--dir", hostDir
    ]
  createDirectoryIfMissing True (mnt </> "etc")
  logInfo $ fromString $ "Copying Nux system configuration to " <> mnt
  void $ exec "cp" ["-r", flake, mnt </> "etc/nuxos"]
  logInfo $ fromString $ "Installing Nux system to " <> mnt
  void $ exec "nixos-install" ["--flake", mnt </> "etc/nuxos#" <> hostname, "--root", mnt]
  logInfo $ fromString $ "Congradulations! Installation succeeded!"
  umount (mnt <> efiMount)
  umount mnt
  where
    mnt = "/mnt"

withTempFlake :: (FilePath -> RIO env ()) -> RIO env ()
withTempFlake = withSystemTempDirectory "nuxos"
