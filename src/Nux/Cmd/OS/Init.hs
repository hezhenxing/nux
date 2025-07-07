{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Init
  ( initCmd
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.OS
import           Nux.User
import           Nux.Util
import           RIO
import           RIO.Directory

data InitOptions = InitOptions
  { initOptPath         :: FilePath
  , initOptProfile      :: String
  , initOptLanguage     :: String
  , initOptTimezone     :: String
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
  "Initialize a new NuxOS configuration"
  runInit
  (InitOptions
    <$> strArgument ( metavar "PATH"
                   <> help "Directory path to create NuxOS configuration"
                    )
    <*> strOption ( long "profile"
                 <> short 'r'
                 <> help "Profile to use"
                 <> value ""
                  )
    <*> strOption ( long "language"
                 <> short 'l'
                 <> help "Language of the system"
                 <> value ""
                  )
    <*> strOption ( long "timezone"
                 <> short 't'
                 <> help "Timezone of the system"
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
  url <- view urlL
  isForce <- view forceL
  system <- view systemL
  hostname <- view hostL
  username <- view userL
  flake <- makeAbsolute initOptPath
  let autos = concatMap (split ',') initOptPackages
  let userAutos = concatMap (split ',') initOptUserPackages
  let filesystems = mempty
  host <- initHost
    system
    initOptProfile
    initOptLanguage
    initOptTimezone
    filesystems
    autos
  user <- initUser
    username
    initOptDescription
    initOptEmail
    initOptUid
    initOptGid
    userAutos
  createFlake flake url hostname host username user isForce
