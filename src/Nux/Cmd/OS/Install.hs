{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Install
  ( installCmd
  )
where

import           Nux.Options
import           Nux.OS
import           Nux.Process
import           Nux.Util      (followLink)
import           RIO
import           RIO.Directory (makeAbsolute)
import           RIO.FilePath
import           SimplePrompt  (yesNo)

data InstallOptions = InstallOptions
  { installOptRoot         :: String
  , installOptFormat       :: Bool
  , installOptLink         :: Bool
  , installOptNoBootLoader :: Bool
  , installOptRootDev      :: String
  }

installCmd :: Command (RIO App ())
installCmd = addCommand
  "install"
  "Install Nux system"
  runInstall
  (InstallOptions
    <$> strOption ( long "root"
                 <> help "Root directory to use when installing NuxOS system"
                 <> value "/mnt"
                 )
    <*> switch ( long "format"
              <> help "Format the root device before installation"
               )
    <*> switch ( long "link"
              <> short 'L'
              <> help "Use symbolic link instead of copying to /etc/nuxos"
               )
    <*> switch ( long "no-bootloader"
              <> short 'B'
              <> help "Do not install boot loader"
               )
    <*> strArgument ( metavar "DEVICE"
                   <> help "The root partition device to install NuxOS configuration directory"
                    )
  )

runInstall :: InstallOptions -> RIO App ()
runInstall InstallOptions{..} = do
  yes <- view yesL
  hostname <- view hostL
  isForce <- view forceL
  flake <- view flakeL >>= makeAbsolute >>= followLink
  let hostDir = flake </> "nix/hosts" </> hostname
  let rootDev = installOptRootDev
  let rootDir = if rootDev == ""
                  then "/"
                  else installOptRoot
  if rootDev == ""
    then
      logInfo $ fromString $ "Will install NuxOS configuration of host " <> hostname <> " from " <> flake <> " to current system"
    else
      logInfo $ fromString $ "Will install NuxOS configuration of host " <> hostname <> " from " <> flake <> " to root device " <> rootDev
  unless yes $ do
    y <- yesNo "Do you want to continue the installation"
    unless y $ die "user cancelled installation!"
  prepareRoot rootDev rootDir installOptFormat isForce
  generateHardwareConfig rootDir hostDir isForce
  generateDrivers hostDir isForce
  installFlake flake rootDir hostname installOptLink installOptNoBootLoader isForce
