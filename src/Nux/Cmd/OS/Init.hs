{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Init
  ( initCmd,
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.OS
import           Nux.User
import           Nux.Util
import           RIO
import           RIO.Directory
import qualified RIO.List      as L

data InitOptions = InitOptions
  { initOptProfile      :: String
  , initOptPackages     :: [String]
  , initOptDescription  :: String
  , initOptEmail        :: String
  , initOptUid          :: Int
  , initOptGid          :: Int
  , initOptUserPackages :: [String]
  }

initCmd :: Command (RIO App ())
initCmd = addCommand
  "init"
  "Initialize NuxOS in the directory"
  runInit
  (InitOptions
    <$> strOption ( long "profile"
                 <> help "Profile to use"
                 <> value ""
                  )
    <*> many (strOption ( long "packages"
                       <> short 'p'
                       <> help "Additional comma separated packages to install, can be specified multiple times"
                       ))
    <*> strOption ( long "description"
                 <> short 'd'
                 <> help "Description of the VM"
                 <> value ""
                 )
    <*> strOption ( long "email"
                 <> short 'e'
                 <> help "Email address of the VM user"
                 <> value ""
                 )
    <*> option auto ( long "uid"
                   <> short 'u'
                   <> help "User ID of the VM user"
                   <> value 0
                   )
    <*> option auto ( long "gid"
                   <> short 'g'
                   <> help "Group ID of the VM user"
                   <> value 0
            )
    <*> many (strOption ( long "user-packages"
                   <> short 'P'
                   <> help "Additional comma separated user packages to install, can be specified multiple times"
                   ))
  )

runInit :: InitOptions -> RIO App ()
runInit InitOptions{..} = do
  flake <- view flakeL
  isForce <- view forceL
  system <- view systemL
  url <- view urlL
  hostname <- view hostL
  username <- view userL
  let hostDir = hostDirPath flake hostname
  let userDir = userDirPath flake username
  logInfo $ fromString $ "Starting NuxOS initialization in " <> flake
  createDirectoryIfMissing True flake
  isEmpty <- isDirectoryEmpty flake
  unless isEmpty $ do
    if isForce
      then do
        logWarn "Directory is not empty, Forcing overwrite existing files..."
      else do
        logError "The target directory is not empty."
        throwString $ "directory not empty: " <> flake
  logInfo $ fromString $ "Initializing Nux in directory " <> flake
  initFlake flake url
  UserInfo{..} <- getUserInfo username
  let autos = concatMap (split ',') initOptPackages
  let usrAutos = concatMap (split ',') initOptUserPackages
  let host = emptyHost { hostSystem = system
                       , hostProfile = initOptProfile
                       , hostAutos = autos
                       }
  let user = emptyUser { userDescription = initOptDescription `nullOr` userInfoDescription
                       , userEmail       = initOptEmail       `nullOr` userInfoEmail
                       , userUid         = Just $ initOptUid  `zeroOr` userInfoUid
                       , userGid         = Just $ initOptGid  `zeroOr` userInfoGid
                       , userAutos       = usrAutos
                       }
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
  logInfo "NuxOS initialized successfully."
