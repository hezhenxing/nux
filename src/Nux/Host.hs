{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Host where

import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L

data FileSystem = FileSystem
  { fsDevice  :: String
  , fsType    :: String
  , fsOptions :: [String]
  } deriving (Show, Eq)

instance FromJSON FileSystem where
  parseJSON = withObject "FileSystem" $ \v -> FileSystem
    <$> v .:  "device"
    <*> v .:? "fsType"  .!= "auto"
    <*> v .:? "options" .!= [ "defaults" ]

instance ToJSON FileSystem where
  toJSON (FileSystem dev typ opts) = object
    [ "device"  .= dev
    , "fsType"  .= typ
    , "options" .= opts
    ]

type FileSystems = Map String FileSystem

data Host = Host
  { hostSystem      :: String
  , hostFileSystems :: FileSystems
  , hostAutos       :: [String]
  , hostModules     :: [String]
  , hostServices    :: [String]
  , hostPrograms    :: [String]
  , hostPackages    :: [String]
  } deriving (Show, Eq)

instance FromJSON Host where
  parseJSON = withObject "Host" $ \v -> Host
    <$> v .:  "system"
    <*> v .:? "fileSystems" .!= mempty
    <*> v .:? "autos"       .!= []
    <*> v .:? "modules"     .!= []
    <*> v .:? "services"    .!= []
    <*> v .:? "programs"    .!= []
    <*> v .:? "packages"    .!= []

instance ToJSON Host where
  toJSON (Host sys fs autos mods svcs progs pkgs) = object
    [ "system"      .= sys
    , "fileSystems" .= fs
    , "autos"       .= autos
    , "modules"     .= mods
    , "services"    .= svcs
    , "programs"    .= progs
    , "packages"    .= pkgs
    ]

type Hosts = Map String Host

hostsDirPath :: FilePath -> FilePath
hostsDirPath flake = flake </> "nix/hosts"

hostDirPath :: FilePath -> String -> FilePath
hostDirPath flake host = hostsDirPath flake </> host

hostNixFilePath :: FilePath -> String -> FilePath
hostNixFilePath flake host = hostDirPath flake host </> "default.nix"

hostFilePath :: FilePath -> String -> FilePath
hostFilePath flake host = hostDirPath flake host </> "host.json"

doesHostExist :: FilePath -> String -> RIO env Bool
doesHostExist flake hostname = doesFileExist $ hostFilePath flake hostname

writeHost :: FilePath -> Host -> RIO env ()
writeHost path host = do
  writeBinaryFile path $ BL.toStrict $ encodePretty host

readHost :: FilePath -> RIO env Host
readHost path = do
  content <- readFileUtf8 path
  case eitherDecodeStrictText content of
    Left err -> throwString $ "Failed to parse host file: " <> err
    Right host -> return host

readFlakeHost :: FilePath -> String -> RIO env Host
readFlakeHost flake hostname = readHost $ hostFilePath flake hostname

writeFlakeHost :: FilePath -> String -> Host -> RIO env ()
writeFlakeHost flake hostname host = writeHost (hostFilePath flake hostname) host

addHostModule :: String -> Host -> Host
addHostModule m host = host
  { hostModules = m : hostModules host
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
delHostModule m host@Host{..} =
  host { hostModules = L.delete m hostModules }

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
