{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Nux.Host where

import RIO
import RIO.File
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.Map as Map

data User = User
  { userUid :: Maybe Int
  , userGid :: Maybe Int
  , userDescription :: String
  , userEmail :: String
  , userAutos :: [String]
  , userModules :: [String]
  , userServices :: [String]
  , userPrograms :: [String]
  , userPackages :: [String]
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .:? "uid"
    <*> v .:? "gid"
    <*> v .: "description"
    <*> v .: "email"
    <*> v .: "autos"
    <*> v .: "modules"
    <*> v .: "services"
    <*> v .: "programs"
    <*> v .: "packages"

instance ToJSON User where
  toJSON (User uid gid desc email autos mods svcs progs pkgs) = object
    [ "uid"         .= uid
    , "gid"         .= gid
    , "description" .= desc
    , "email"       .= email
    , "autos"       .= autos
    , "modules"     .= mods
    , "services"    .= svcs
    , "programs"    .= progs
    , "packages"    .= pkgs
    ]

type Users = Map String User

data FileSystem = FileSystem
  { fsDevice :: String
  , fsType   :: String
  , fsOptions :: [String]
  } deriving (Show, Eq)

instance FromJSON FileSystem where
  parseJSON = withObject "FileSystem" $ \v -> FileSystem
    <$> v .: "device"
    <*> v .: "fsType"
    <*> v .: "options"

instance ToJSON FileSystem where
  toJSON (FileSystem dev typ opts) = object
    [ "device" .= dev
    , "fsType" .= typ
    , "options" .= opts
    ]

type FileSystems = Map String FileSystem

data Host = Host
  { hostSystem :: String
  , hostFileSystems :: FileSystems
  , hostAutos :: [String]
  , hostModules :: [String]
  , hostServices :: [String]
  , hostPrograms :: [String]
  , hostPackages :: [String]
  , hostUsers :: Users
  } deriving (Show, Eq)

instance FromJSON Host where
  parseJSON = withObject "Host" $ \v -> Host
    <$> v .: "system"
    <*> v .: "fileSystems"
    <*> v .: "autos"
    <*> v .: "modules"
    <*> v .: "services"
    <*> v .: "programs"
    <*> v .: "packages"
    <*> v .: "users"

instance ToJSON Host where
  toJSON (Host sys fs autos mods svcs progs pkgs users) = object
    [ "system" .= sys
    , "fileSystems" .= fs
    , "autos" .= autos
    , "modules" .= mods
    , "services" .= svcs
    , "programs" .= progs
    , "packages" .= pkgs
    , "users" .= users
    ]

type Hosts = Map String Host

writeHost :: FilePath -> Host -> RIO env ()
writeHost path host = do
  writeBinaryFile path $ BL.toStrict $ encodePretty host

readHost :: FilePath -> RIO env Host
readHost path = do
  content <- readFileUtf8 path
  case eitherDecodeStrictText content of
    Left err -> throwString $ "Failed to parse host file: " <> err
    Right host -> return host

exitHostModule :: String -> Host -> Bool
exitHostModule mod host = mod `elem` hostModules host

addHostModule :: String -> Host -> Host
addHostModule mod host = host
  { hostModules = mod : hostModules host
  }

addHostService :: String -> Host -> Host
addHostService svc host = host
  { hostServices = svc : hostServices host
  }

addHostProgram :: String -> Host -> Host
addHostProgram prog host = host
  { hostPrograms = prog : hostPrograms host
  }

addHostPackage :: String -> Host -> Host
addHostPackage pkg host = host
  { hostPackages = pkg : hostPackages host
  }

addHostAuto :: String -> Host -> Host
addHostAuto auto host = host
  { hostAutos = auto : hostAutos host
  }

exitUserModule :: String -> String -> Host -> Bool
exitUserModule mod userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> mod `elem` userModules user
    Nothing   -> False

addUserModule :: String -> String -> Host -> Host
addUserModule mod userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> host
      { hostUsers = Map.insert userName (user { userModules = mod : userModules user }) (hostUsers host) }
    Nothing -> host -- User not found, do nothing

addUserService :: String -> String -> Host -> Host
addUserService svc userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> host
      { hostUsers = Map.insert userName (user { userServices = svc : userServices user }) (hostUsers host) }
    Nothing -> host -- User not found, do nothing

addUserProgram :: String -> String -> Host -> Host
addUserProgram prog userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> host
      { hostUsers = Map.insert userName (user { userPrograms = prog : userPrograms user }) (hostUsers host) }
    Nothing -> host -- User not found, do nothing

addUserPackage :: String -> String -> Host -> Host
addUserPackage pkg userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> host
      { hostUsers = Map.insert userName (user { userPackages = pkg : userPackages user }) (hostUsers host) }
    Nothing -> host -- User not found, do nothing

addUserAuto :: String -> String -> Host -> Host
addUserAuto auto userName host =
  case Map.lookup userName (hostUsers host) of
    Just user -> host
      { hostUsers = Map.insert userName (user { userAutos = auto : userAutos user }) (hostUsers host) }
    Nothing -> host -- User not found, do nothing

delHostModule :: String -> Host -> Host
delHostModule mod host@Host{..} =
  host { hostModules = L.delete mod hostModules }

delHostService :: String -> Host -> Host
delHostService svc host@Host{..} =
  host { hostServices = L.delete svc hostServices }

delHostProgram :: String -> Host -> Host
delHostProgram prog host@Host{..} =
  host { hostPrograms = L.delete prog hostPrograms }

delHostPackage :: String -> Host -> Host
delHostPackage pkg host@Host{..} =
  host { hostPackages = L.delete pkg hostPackages }

delHostAuto :: String -> Host -> Host
delHostAuto auto host@Host{..} =
  host { hostAutos = L.delete auto hostAutos }

delUserModule :: String -> String -> Host -> Host
delUserModule mod userName host@Host{..} =
  case Map.lookup userName hostUsers of
    Just user -> host
      { hostUsers = Map.insert userName (user { userModules = mod: userPackages user}) hostUsers }
    Nothing -> host -- User not found, do nothing

delUserService :: String -> String -> Host -> Host
delUserService svc userName host@Host{..} =
  case Map.lookup userName hostUsers of
    Just user -> host
      { hostUsers = Map.insert userName (user { userServices = svc : userServices user}) hostUsers}
    Nothing -> host -- User not found, do nothing

delUserProgram :: String -> String -> Host -> Host
delUserProgram prog userName host@Host{..} =
  case Map.lookup userName hostUsers of
    Just user -> host
      { hostUsers = Map.insert userName (user {userPrograms = prog : userPrograms user}) hostUsers}
    Nothing -> host -- User not found, do nothing

delUserPackage :: String -> String -> Host -> Host
delUserPackage pkg userName host@Host{..} =
  case Map.lookup userName hostUsers of
    Just user -> host
      { hostUsers = Map.insert userName (user {userPrograms = pkg : userPackages user}) hostUsers}
    Nothing -> host -- User not found, do nothing

delUserAuto :: String -> String -> Host -> Host
delUserAuto auto userName host@Host{..} =
  case Map.lookup userName hostUsers of
    Just user -> host
      { hostUsers = Map.insert userName (user {userAutos = auto : userAutos user}) hostUsers}
    Nothing -> host -- User not found, do nothing

emptyHost :: Host
emptyHost = Host
  { hostSystem = ""
  , hostFileSystems = mempty
  , hostAutos = []
  , hostModules = []
  , hostServices = []
  , hostPrograms = []
  , hostPackages = []
  , hostUsers = mempty
  }

newHost :: String -> Host
newHost system = emptyHost { hostSystem = system }

nuxosHost :: String -> Host
nuxosHost system = emptyHost
  { hostSystem = system
  , hostUsers = Map.singleton "nux" nuxUser
  }

emptyUser :: User
emptyUser = User
  { userUid = Nothing
  , userGid = Nothing
  , userDescription = ""
  , userEmail = ""
  , userAutos = []
  , userModules = []
  , userServices = []
  , userPrograms = []
  , userPackages = []
  }

newUser :: String -> String -> User
newUser desc email = emptyUser
  { userDescription = desc
  , userEmail = email
  }

nuxUser :: User
nuxUser = newUser "Nux User" "nux@localhost"
