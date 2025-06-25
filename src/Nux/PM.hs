{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.PM where

import           Data.Aeson
import           Nux.Host
import           Nux.User
import           Nux.Util
import           RIO
import qualified RIO.List   as L
import qualified RIO.Map    as Map

data OptionItem = OptionItem
  { oiVisible :: Bool
  , oiEnable  :: Bool
  } deriving (Show)

instance FromJSON OptionItem where
  parseJSON = withObject "OptionItem" $ \v -> OptionItem
    <$> v .: "visible"
    <*> v .: "enable"

instance ToJSON OptionItem where
  toJSON (OptionItem visible enable) = object
    [ "visible" .= visible
    , "enable"  .= enable
    ]

nixosModules :: FilePath -> RIO env [String]
nixosModules flake = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nuxos.nixosModules"
               , "--apply"
               , "builtins.attrNames"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

homeModules :: FilePath -> RIO env [String]
homeModules flake = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nuxos.homeModules"
               , "--apply"
               , "builtins.attrNames"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosOptItems :: FilePath -> String -> String -> RIO env (Map String OptionItem)
nixosOptItems flake host opt = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nixosConfigurations." <> host <> ".options." <> opt
               , "--apply"
               , "with builtins; mapAttrs (n: v: rec { visible = v.visible or v.enable.visible or true; enable = if visible then v.enable.value or false else false; })"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosServices :: FilePath -> String -> RIO env (Map String OptionItem)
nixosServices flake host = nixosOptItems flake host "services"

nixosPrograms :: FilePath -> String -> RIO env (Map String OptionItem)
nixosPrograms flake host = nixosOptItems flake host "programs"

nixosPackages :: FilePath -> String -> RIO env [String]
nixosPackages flake host = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nixosConfigurations." <> host <> ".pkgs"
               , "--apply"
               , "builtins.attrNames"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosSystemPackages :: FilePath -> String -> RIO env [String]
nixosSystemPackages flake host = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nixosConfigurations." <> host <> ".config.environment.systemPackages"
               , "--apply"
               , "with builtins; map (p: p.name)"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosHomePackages :: FilePath -> String -> String -> RIO env [String]
nixosHomePackages flake host user = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nixosConfigurations." <> host <> ".config.home-manager.users." <> user <> ".home.packages"
               , "--apply"
               , "with builtins; map (p: p.name)"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosHomeItems :: FilePath -> String -> String -> String -> RIO env (Map String OptionItem)
nixosHomeItems flake host user opt = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#nixosConfigurations." <> host <> ".config.home-manager.users." <> user <> "." <> opt
               , "--apply"
               , "with builtins; mapAttrs (n: v: rec { visible = v.visible or v.enable.visible or true; enable = if visible then v.enable.value or false else false; })"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

nixosHomeServices :: FilePath -> String -> String -> RIO env (Map String OptionItem)
nixosHomeServices flake host user = nixosHomeItems flake host user "services"

nixosHomePrograms :: FilePath -> String -> String -> RIO env (Map String OptionItem)
nixosHomePrograms flake host user = nixosHomeItems flake host user "programs"

homeItems :: FilePath -> String -> String -> RIO env (Map String OptionItem)
homeItems flake user opt = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#homeConfigurations." <> user <> ".options." <> opt
               , "--apply"
               , "with builtins; mapAttrs (n: v: rec { visible = v.visible or v.enable.visible or true; enable = if visible then v.enable.value or false else false; })"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

homeServices :: FilePath -> String -> RIO env (Map String OptionItem)
homeServices flake user = homeItems flake user "services"

homePrograms :: FilePath -> String -> RIO env (Map String OptionItem)
homePrograms flake user = homeItems flake user "programs"

homePackages :: FilePath -> String -> RIO env [String]
homePackages flake user = do
  content <-
    nix "eval" [ "--json"
               , flake <> "#homeConfigurations." <> user <> ".pkgs"
               , "--apply"
               , "builtins.attrNames"
               ]
  case eitherDecode (fromString content) of
    Left err -> throwString err
    Right r  -> return r

addHostAutos :: FilePath -> String -> [String] -> RIO env ()
addHostAutos flake hostname names = do
  let hostFile = hostFilePath flake hostname
  host <- readHost hostFile
  modules <- nixosModules flake
  services <- nixosServices flake hostname
  programs <- nixosPrograms flake hostname
  packages <- nixosPackages flake hostname
  let
    go (h, unknowns) name =
      if name `elem` modules
        || Map.member name services
        || Map.member name programs
        || name `elem` packages
      then
        (addHostAuto name h, unknowns)
      else
        (h, name:unknowns)
  let (host', unknowns) = foldl' go (host, []) (L.nub names)
  if unknowns /= []
  then do
    throwString $ fromString $ "system packages not found: " <> show unknowns
  else do
    writeHost hostFile host'

delHostAutos :: FilePath -> String -> [String] -> RIO env ()
delHostAutos flake hostname names = do
  host <- readFlakeHost flake hostname
  let
    go (h, unknowns) name =
      if name `elem` hostAutos h
      then (h { hostAutos = L.delete name (hostAutos h) }, unknowns)
      else (h, name : unknowns)
  let (host', unknowns) = foldl' go (host, []) (L.nub names)
  if unknowns /= []
  then
    throwString $ fromString $ "system packages not found: " <> show unknowns
  else
    writeFlakeHost flake hostname host'

addUserAutos :: FilePath -> String -> [String] -> RIO env ()
addUserAutos flake username names = do
  let userFile = userFilePath flake username
  user <- readUser userFile
  modules <- homeModules flake
  services <- homeServices flake username
  programs <- homePrograms flake username
  packages <- homePackages flake username
  let
    go (u, unknowns) name =
      if name `elem` modules
        || Map.member name services
        || Map.member name programs
        || name `elem` packages
      then
        (addUserAuto name u, unknowns)
      else
        (u, name:unknowns)
  let (user', unknowns) = foldl' go (user, []) (L.nub names)
  if unknowns /= []
  then do
    throwString $ fromString $ "user packages not found: " <> show unknowns
  else do
    writeUser userFile user'

delUserAutos :: FilePath -> String -> [String] -> RIO env ()
delUserAutos flake username names = do
  user <- readFlakeUser flake username
  let
    go (u, unknowns) name =
      if name `elem` userAutos u
      then (u { userAutos = L.delete name (userAutos u) }, unknowns)
      else (u, name : unknowns)
  let (user', unknowns) = foldl' go (user, []) (L.nub names)
  if unknowns /= []
  then
    throwString $ fromString $ "user packages not found: " <> show unknowns
  else
    writeFlakeUser flake username user'

listHostAutos :: HasLogFunc env => FilePath -> String -> RIO env ()
listHostAutos flake hostname = do
  host <- readFlakeHost flake hostname
  forM_ (hostAutos host) $ \auto -> do
    logInfo $ fromString $ "  " <> auto

listUserAutos :: HasLogFunc env => FilePath -> String -> RIO env ()
listUserAutos flake username = do
  user <- readFlakeUser flake username
  forM_ (userAutos user) $ \auto -> do
    logInfo $ fromString $ "  " <> auto
