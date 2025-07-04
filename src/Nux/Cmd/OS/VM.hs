{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.VM
  ( vmCmd
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.OS
import           Nux.User
import           Nux.Util
import           RIO
import qualified RIO.ByteString as B
import qualified RIO.List       as L

data VmOptions = VmOptions
  { vmOptFromScratch  :: Bool
  , vmOptWait         :: Bool
  , vmOptDescription  :: String
  , vmOptEmail        :: String
  , vmOptUid          :: Int
  , vmOptGid          :: Int
  , vmOptPackages     :: [String]
  , vmOptUserPackages :: [String]
  }

vmCmd :: Command (RIO App ())
vmCmd = addCommand
  "vm"
  "Build and run virtual machine from the NuxOS configuration"
  runVM
  (VmOptions
    <$> switch ( long "from-scratch"
              <> help "Build NuxOS VM from scratch, otherwise use the existing configuration"
              )
    <*> switch ( long "wait"
              <> short 'w'
              <> help "Wait before launching the vm from scratch"
               )
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
    <*> many (strOption ( long "packages"
                   <> short 'p'
                   <> metavar "PACKAGES"
                   <> help "Additional comma separated packages to install, can be specified multiple times"
                   ))
    <*> many (strOption ( long "user-packages"
                   <> short 'P'
                   <> metavar "PACKAGES"
                   <> help "Additional comma separated user packages to install, can be specified multiple times"
                   ))
  )

runVM :: VmOptions -> RIO App ()
runVM opts = do
  if vmOptFromScratch opts
    then do
      system <- view systemL
      hostname <- view hostL
      url <- view urlL
      username <- view userL
      uinfo <- getUserInfo username
      let autos = concatMap (split ',') (vmOptPackages opts)
      let usrAutos = concatMap (split ',') (vmOptUserPackages opts)
      let host = emptyHost { hostSystem = system
                           , hostAutos = autos
                           }
      let user = emptyUser { userDescription = vmOptDescription opts `nullOr` userInfoDescription uinfo
                           , userEmail       = vmOptEmail       opts `nullOr` userInfoEmail uinfo
                           , userUid         = vmOptUid         opts `zeroOr` userInfoUid uinfo
                           , userGid         = vmOptGid         opts `zeroOr` userInfoGid uinfo
                           , userAutos       = usrAutos
                           }
      withTempFlake $ \flake -> do
        let hostDir = hostDirPath flake hostname
        let userDir = userDirPath flake username
        logInfo $ fromString $ "Building NuxOS VM from scratch with flake at " <> flake
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
        when (vmOptWait opts) $ do
          logInfo "Press enter to continue..."
          void B.getLine
        vmRunFlake flake hostname
    else do
      flake <- view flakeL
      hostname <- view hostL
      vmRunFlake flake hostname

vmRunFlake :: HasLogFunc env => FilePath -> String -> RIO env ()
vmRunFlake flake hostname = do
  logInfo $ fromString $ "Building NuxOS VM of " <> hostname <> " from flake " <> flake
  tryAny (nixrun [flake <> "#nixosConfigurations." <> hostname <> ".config.system.build.vm"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to start vm: " <> displayException e
    Right _ -> do
      logInfo "Start VM of NuxOS configuration successfully."
