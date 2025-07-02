{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Host where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           RIO
import qualified RIO.ByteString.Lazy      as BL
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List                 as L

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
  , hostProfile     :: String
  , hostFileSystems :: FileSystems
  , hostAutos       :: [String]
  } deriving (Show, Eq)

instance FromJSON Host where
  parseJSON = withObject "Host" $ \v -> Host
    <$> v .:  "system"
    <*> v .:? "profile"     .!= ""
    <*> v .:? "fileSystems" .!= mempty
    <*> v .:? "autos"       .!= []

instance ToJSON Host where
  toJSON (Host sys prof fs autos) = object
    [ "system"      .= sys
    , "profile"     .= prof
    , "fileSystems" .= fs
    , "autos"       .= autos
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
    Left err   -> throwString $ "Failed to parse host file: " <> err
    Right host -> return host

readFlakeHost :: FilePath -> String -> RIO env Host
readFlakeHost flake hostname = readHost $ hostFilePath flake hostname

writeFlakeHost :: FilePath -> String -> Host -> RIO env ()
writeFlakeHost flake hostname = writeHost (hostFilePath flake hostname)

writeFlakeHostNix :: FilePath -> String -> RIO env ()
writeFlakeHostNix flake hostname = do
  let hostNix = hostNixFilePath flake hostname
  writeBinaryFile hostNix
    $ fromString
    $ L.unlines
      [ "with builtins; fromJSON (readFile ./host.json)"
      ]

addFlakeHost :: FilePath -> String -> Host -> RIO env ()
addFlakeHost flake hostname host = do
  let hostDir = hostDirPath flake hostname
  createDirectoryIfMissing True hostDir
  writeFlakeHostNix flake hostname
  writeFlakeHost flake hostname host

addHostAuto :: String -> Host -> Host
addHostAuto auto host = host
  { hostAutos = auto : hostAutos host
  }

delHostAuto :: String -> Host -> Host
delHostAuto auto host@Host{..} =
  host { hostAutos = L.delete auto hostAutos }

emptyHost :: Host
emptyHost = Host
  { hostSystem = ""
  , hostProfile = ""
  , hostFileSystems = mempty
  , hostAutos = []
  }

newHost :: String -> Host
newHost system = emptyHost { hostSystem = system }

nuxosHost :: String -> Host
nuxosHost system = emptyHost
  { hostSystem = system
  }
