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
import           RIO
import           RIO.Directory (makeAbsolute)
import           RIO.FilePath
import           SimplePrompt  (yesNo)

data InstallOptions = InstallOptions
  { installOptRootDev  :: String
  , installOptGenerate :: Bool
  , installOptFormat   :: Bool
  , installOptLink     :: Bool
  , installOptFlake    :: String
  }

installCmd :: Command (RIO App ())
installCmd = addCommand
  "install"
  "Install Nux system"
  runInstall
  (InstallOptions
    <$> strOption ( long "root"
                 <> help "Root device to install Nux system"
                 <> value ""
                 )
    <*> switch ( long "generate"
              <> short 'g'
              <> help "Generate hardware configuration even if it already exists"
               )
    <*> switch ( long "format"
              <> help "Format the root device before installation"
               )
    <*> switch ( long "link"
              <> short 'L'
              <> help "Use symbolic link instead of copying to /etc/nuxos"
               )
    <*> strArgument ( metavar "DIR"
                   <> help "The NuxOS configuration directory, default to current directory"
                   <> value "."
                    )
  )

runInstall :: InstallOptions -> RIO App ()
runInstall InstallOptions{..} = do
  yes <- view yesL
  hostname <- view hostL
  isForce <- view forceL
  flake <- makeAbsolute installOptFlake
  let hostDir = flake </> "nix/hosts" </> hostname
  let rootDev = installOptRootDev
  if rootDev == ""
    then
      logInfo $ fromString $ "Will install NuxOS system from flake " <> flake <> " to current system"
    else
      logInfo $ fromString $ "Will install NuxOS system from flake " <> flake <> " to root device " <> rootDev
  unless yes $ do
    y <- yesNo "Do you want to continue the installation"
    unless y $ die "user cancelled installation!"
  rootDir <- prepareRoot rootDev installOptFormat isForce
  generateHardwareConfig rootDir hostDir installOptGenerate
  generateDrivers hostDir installOptGenerate
  installFlake flake rootDir hostname installOptLink isForce
