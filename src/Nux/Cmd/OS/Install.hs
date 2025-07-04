{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Install
  ( installCmd,
  )
where

import           Nux.Host
import           Nux.Options
import           Nux.OS
import           Nux.User
import           Nux.Util
import           RIO
import qualified RIO.List     as L
import           SimplePrompt (yesNo)

data InstallOptions = InstallOptions
  { installOptRootDev      :: String
  , installOptFromScratch  :: Bool
  , installOptYes          :: Bool
  , installOptFormat       :: Bool
  , installOptPackages     :: [String]
  , installOptDescription  :: String
  , installOptEmail        :: String
  , installOptUid          :: Int
  , installOptGid          :: Int
  , installOptUserPackages :: [String]
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
    <*> switch ( long "from-scratch"
              <> short 's'
              <> help "Install Nux system from scratch without using a flake"
               )
    <*> switch ( long "yes"
              <> short 'y'
              <> help "Assume yes for all prompts asking for confirmation"
               )
    <*> switch ( long "format"
              <> help "Format the root device before installation"
               )
    <*> many (strOption ( long "package"
                       <> short 'p'
                       <> metavar "PACKAGES"
                       <> help "Additional comma separated packages to install, can be specified multiple times"
                       ))
    <*> strOption ( long "description"
                 <> short 'd'
                 <> metavar "DESCRIPTION"
                 <> help "Description of the VM"
                 <> value ""
                 )
    <*> strOption ( long "email"
                 <> short 'e'
                 <> metavar "EMAIL"
                 <> help "Email address of the VM user"
                 <> value ""
                 )
    <*> option auto ( long "uid"
                   <> short 'u'
                   <> metavar "UID"
                   <> help "User ID of the VM user"
                   <> value 0
                   )
    <*> option auto ( long "gid"
                   <> short 'g'
                   <> metavar "GID"
                   <> help "Group ID of the VM user"
                   <> value 0
            )
    <*> many (strOption ( long "user-packages"
                   <> short 'P'
                   <> metavar "PACKAGES"
                   <> help "Additional comma separated user packages to install, can be specified multiple times"
                   ))
  )

runInstall :: InstallOptions -> RIO App ()
runInstall opts = do
  if installOptFromScratch opts
    then installFromScratch opts
    else installFromFlake opts

installFromFlake :: InstallOptions -> RIO App ()
installFromFlake InstallOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  isForce <- view forceL
  let rootDev = installOptRootDev
  if rootDev == ""
    then
      logInfo $ fromString $ "Will install NuxOS system from flake " <> flake <> " to current system"
    else
      logInfo $ fromString $ "Will install NuxOS system from flake " <> flake <> " to root device " <> rootDev
  unless installOptYes $ do
    yes <- yesNo "Do you want to continue the installation?"
    unless yes $ throwString "user cancelled installation!"
  installFlake flake rootDev hostname installOptFormat isForce

installFromScratch :: InstallOptions -> RIO App ()
installFromScratch InstallOptions{..} = do
  isForce <- view forceL
  system <- view systemL
  url <- view urlL
  hostname <- view hostL
  username <- view userL
  UserInfo{..} <- getUserInfo username
  let autos = concatMap (split ',') installOptPackages
  let usrAutos = concatMap (split ',') installOptUserPackages
  let rootDev = installOptRootDev
  let host = emptyHost { hostSystem = system
                       , hostAutos = autos
                       }
  let user = emptyUser { userDescription = installOptDescription `nullOr` userInfoDescription
                       , userEmail       = installOptEmail       `nullOr` userInfoEmail
                       , userUid         = installOptUid         `zeroOr` userInfoUid
                       , userGid         = installOptGid         `zeroOr` userInfoGid
                       , userAutos       = usrAutos
                       }
  withTempFlake $ \flake -> do
    let hostDir = hostDirPath flake hostname
    let userDir = userDirPath flake username
    logInfo $ fromString $ "Using temporary flake at " <> flake
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
    logInfo $ fromString $ "Generated NuxOS configuration in " <> flake
    unless installOptYes $ do
      yes <- yesNo "Do you want to continue the installation?"
      unless yes $
        throwString "user cancelled operation!"
    logInfo $ fromString $ "Installing Nux system from flake " <> flake
    installFlake flake rootDev hostname installOptFormat isForce
