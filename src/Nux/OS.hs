{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.OS where

import           Nux.Host
import           Nux.Process
import           Nux.User
import           Nux.Util
import           RIO
import qualified RIO.ByteString.Lazy as BL
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List            as L
import           RIO.Process
import           System.IO           (readFile)

writeFlakeFile :: FilePath -> String -> RIO env ()
writeFlakeFile flake url = do
  let flakeFile = flake </> "flake.nix"
  writeBinaryFile flakeFile $ fromString $ L.unlines
    [ "{"
    , "  inputs.nuxos.url = \"" <> url <> "\";"
    , "  outputs = inputs: inputs.nuxos ./. {"
    , "    inherit inputs;"
    , "  };"
    , "}"
    ]

initFlake
 :: HasLogFunc env
 => FilePath     -- flake
 -> String       -- url
 -> String       -- hostname
 -> Host
 -> String       -- username
 -> User
 -> Bool         -- force
 -> RIO env ()
initFlake flake url hostname host username user isForce = do
  logInfo $ fromString $ "Initializing NuxOS configuration directory " <> flake
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
  writeFlakeFile flake url
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
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath          -- path to the flake directory
  -> FilePath          -- root directory (e.g., "/" or "/mnt")
  -> String            -- hostname
  -> Bool              -- symLink
  -> Bool              -- force
  -> RIO env ()
installFlake flake rootDir hostname symLink isForce = do
  let lockFile = flake </> "flake.lock"
  unlessM (doesFileExist lockFile) $
    run $ cmd "nix" & arg "flake" & arg "lock"
  exists <- doesPathExist $ rootDir </> "etc/nuxos"
  when (exists && not isForce) $ do
    throwString "target root filesystem already has NuxOS installed, use --force if you want to overwrite it"
  let nuxosPath = rootDir </> "etc/nuxos"
  run $ cmd "mkdir"
      & arg "-p"
      & arg (rootDir </> "etc")
      & sudo
  run $ cmd "rm"
      & arg "-rf"
      & arg nuxosPath
      & sudo
  if symLink
    then do
      logInfo $ fromString $ "Creating symbolic link of NuxOS configuation at " <> nuxosPath
      run $ cmd "ln"
          & arg "-s"
          & arg flake
          & arg nuxosPath
          & sudo
    else do
      logInfo $ fromString $ "Copying NuxOS configuration to " <> nuxosPath
      run $ cmd "cp"
          & arg "-r"
          & arg flake
          & arg nuxosPath
          & sudo
  logInfo $ fromString $ "Installing NuxOS to " <> rootDir
  if rootDir == "/"
    then do
      -- FIXME: nh does not support --install-bootloader
      run $ cmd "nixos-rebuild"
          & arg "switch"
          & arg "--install-bootloader"
          & arg "--flake"
          & arg (flake <> "#" <> hostname)
          & sudo
    else do
      flakeInstall mnt flake hostname
      umountRoot
  logInfo "Congradulations! Installation succeeded!"
  where
    mnt = "/mnt"

prepareRoot
  :: (HasProcessContext env, HasLogFunc env)
  => String -> Bool -> Bool -> RIO env FilePath
prepareRoot rootDev formatRoot isForce = do
  if rootDev == ""
    then do
      logInfo "Using current root filesystem"
      return "/"
    else do
      size <- blockDevSize rootDev
      if size < 10 * 1024 * 1024 * 1024  -- 10GB
        then throwString "root device size is too small (less than 10GB)"
        else do
          logInfo $ fromString $ "Preparing root filesystem from device " <> rootDev
          checkMountPoint
          when formatRoot $ do
            logInfo $ fromString $ "Formatting " <> rootDev
            mkfs isForce "btrfs" rootDev
          mountRoot rootDev
          return mnt
  where
    mnt = "/mnt"

checkMountPoint
  :: (HasProcessContext env, HasLogFunc env) => RIO env ()
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

mountRoot :: (HasProcessContext env, HasLogFunc env) => String -> RIO env ()
mountRoot rootDev = do
  efiDev <- getEfiDevice
  whenM (mounted rootDev) $ do
    throwString $ "root device already mounted: " <> rootDev
  logInfo $ fromString $ "Mounting " <> rootDev <> " to " <> mnt
  mount rootDev mnt
  run $ cmd "mkdir"
      & arg "-p"
      & arg efiMount
      & sudo
  logInfo $ fromString $ "Mounting " <> efiDev <> " to " <> efiMount
  mount efiDev efiMount
  where
    mnt = "/mnt"
    efiMount = mnt <> "/boot/efi"

umountRoot :: (HasProcessContext env, HasLogFunc env) => RIO env ()
umountRoot = do
  umount efiMount
  umount mnt
  where
    mnt = "/mnt"
    efiMount = mnt <> "/boot/efi"

generateHardwareConfig
  :: (HasProcessContext env, HasLogFunc env)
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
        readStdout
          $ cmd "nixos-generate-config"
          & arg "--show-hardware-config"
          & args (if rootDir == "/" then [] else ["--root", rootDir])
          & sudo
      writeBinaryFile hwFile $ BL.toStrict hwConfig

withTempFlake :: (FilePath -> RIO env ()) -> RIO env ()
withTempFlake = withSystemTempDirectory "nuxos"

generateDrivers
  :: HasLogFunc env
  => FilePath
  -> Bool
  -> RIO env ()
generateDrivers hostDir isForce = do
  let drvFile = hostDir </> "drivers.nix"
  exists <- doesFileExist drvFile
  if exists && not isForce
    then do
      logWarn "Drivers config already exists, add --generate to re-generate"
    else do
      logInfo $ fromString "Generating drivers configuration"
      devices <- listDirectory "/sys/bus/pci/devices"
      drivers <- L.nub . concat <$> mapM deviceDrivers devices
      unless (null drivers) $ do
        logInfo $ fromString $ "Detected video drivers: " <> L.intercalate ", " drivers
        writeBinaryFile drvFile $ fromString $ L.unlines
          [ "{"
          , "  services.xserver.videoDrivers = [" <> L.intercalate " " (map show drivers) <> "];"
          , if "nvidia" `elem` drivers
              then "  hardware.nvidia.open = false;"
              else ""
          , "}"
          ]
  where
    deviceDrivers dev = do
      vendor <- trim <$> liftIO (readFile $ "/sys/bus/pci/devices" </> dev </> "vendor")
      klass  <- trim <$> liftIO (readFile $ "/sys/bus/pci/devices" </> dev </> "class")
      if  "0x03" `L.isPrefixOf` klass
        then case vendor of
          "0x10de" -> return ["nvidia"]
          "0x8086" -> return ["intel"]
          "0x1002" -> return ["amdgpu"]
          _        -> return []
        else return []
