{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Host where

import RIO
import RIO.File
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.Map as Map

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

instance ToJSON Host where
  toJSON (Host sys fs autos mods svcs progs pkgs) = object
    [ "system" .= sys
    , "fileSystems" .= fs
    , "autos" .= autos
    , "modules" .= mods
    , "services" .= svcs
    , "programs" .= progs
    , "packages" .= pkgs
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

emptyHost :: Host
emptyHost = Host
  { hostSystem = ""
  , hostFileSystems = mempty
  , hostAutos = []
  , hostModules = []
  , hostServices = []
  , hostPrograms = []
  , hostPackages = []
  }

newHost :: String -> Host
newHost system = emptyHost { hostSystem = system }

nuxosHost :: String -> Host
nuxosHost system = emptyHost
  { hostSystem = system
  }
