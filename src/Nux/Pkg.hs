{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Nux.Pkg where

import           Data.Aeson
import           Nux.Host
import           Nux.Process
import           Nux.User
import           RIO
import qualified RIO.List    as L
import qualified RIO.Map     as Map
import           RIO.Process

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

applyAttrNames :: Proc -> Proc
applyAttrNames p = p & arg "--apply" & arg "builtins.attrNames"

applyMapNames :: Proc -> Proc
applyMapNames p = p & arg "--apply" & arg "map (p: p.name)"

nixEvalJson :: Proc
nixEvalJson = cmd "nix" & arg "eval" & arg "--json"

nixEvalAttrNames
  :: (HasProcessContext env, HasLogFunc env)
  => String -> FilePath -> RIO env [String]
nixEvalAttrNames attrname flakeDir = do
  content <- nixEvalJson
    & applyAttrNames
    & arg (flakeDir <> "#" <> attrname)
    & readStdout
  case eitherDecode content of
    Left err -> throwString err
    Right r  -> return r

nixEvalMapNames
  :: (HasProcessContext env, HasLogFunc env)
  => String -> FilePath -> RIO env [String]
nixEvalMapNames attrname flakeDir = do
  content <- nixEvalJson
    & applyMapNames
    & arg (flakeDir <> "#" <> attrname)
    & readStdout
  case eitherDecode content of
    Left err -> throwString err
    Right r  -> return r

nixosModules :: (HasProcessContext env, HasLogFunc env) => FilePath -> RIO env [String]
nixosModules = nixEvalAttrNames "nuxos.nixosModules"

homeModules :: (HasProcessContext env, HasLogFunc env) => FilePath -> RIO env [String]
homeModules = nixEvalAttrNames "nuxos.homeModules"

applyMapAttrsOption :: Proc -> Proc
applyMapAttrsOption p =
  p & arg "--apply"
    & arg "with builtins; mapAttrs (n: v: rec { visible = v.visible or v.enable.visible or (hasAttr \"enable\" v); enable = if visible then v.enable.value or false else false; })"

nixosOptStr :: FilePath -> String -> String -> String
nixosOptStr flake host name =
  flake <> ".nixosConfigurations." <> host <> ".options." <> name

nixosOptItems
  :: (HasProcessContext env, HasLogFunc env)
  => String -> RIO env (Map String OptionItem)
nixosOptItems name = do
  content <- readStdout
    $ nixEvalJson
    & applyMapAttrsOption
    & arg name
  case eitherDecode content of
    Left err -> throwString err
    Right r  -> return (Map.filter oiVisible r)

nixosServices
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env (Map String OptionItem)
nixosServices flake host = nixosOptItems $ nixosOptStr flake host "services"

nixosPrograms
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env (Map String OptionItem)
nixosPrograms flake host = nixosOptItems $ nixosOptStr flake host "programs"

nixosPackages
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env [String]
nixosPackages flake host =
  nixEvalAttrNames ("nixosConfigurations." <> host <> ".pkgs") flake

nixosSystemPackages
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env [String]
nixosSystemPackages flake host =
  nixEvalMapNames ("nixosConfigurations." <> host <> ".config.environment.systemPackages") flake

nixosHomePackages
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env [String]
nixosHomePackages flake host user = do
  nixEvalMapNames ("nixosConfigurations." <> host <> ".config.home-manager.users." <> user <> ".home.packages") flake

hmUserOptStr :: FilePath -> String -> String -> String -> String
hmUserOptStr flake host user name
  = flake <> "#nixosConfigurations." <> host <> ".config.home-manager.users." <> user <> "." <> name

nixosHomeItems
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> String -> RIO env (Map String OptionItem)
nixosHomeItems flake host user name =
  nixosOptItems $ hmUserOptStr flake host user name

nixosHomeServices
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env (Map String OptionItem)
nixosHomeServices flake host user =
  nixosHomeItems flake host user "services"

nixosHomePrograms
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env (Map String OptionItem)
nixosHomePrograms flake host user =
  nixosHomeItems flake host user "programs"

homeOptStr :: FilePath -> String -> String -> String
homeOptStr flake user name =
  flake <> "#homeConfigurations." <> user <> ".options." <> name

homeItems
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env (Map String OptionItem)
homeItems flake user name =
  nixosOptItems $ homeOptStr flake user name

homeServices
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env (Map String OptionItem)
homeServices flake user = homeItems flake user "services"

homePrograms
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env (Map String OptionItem)
homePrograms flake user = homeItems flake user "programs"

homePkgsStr :: String -> String
homePkgsStr user =
  "homeConfigurations." <> user <> ".pkgs"

homePackages
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> RIO env [String]
homePackages flake user =
  nixEvalAttrNames (homePkgsStr user) flake

addHostAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> [String] -> RIO env ()
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

addUserAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> [String] -> RIO env ()
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

searchHost
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env ()
searchHost flake hostname query = do
  modules <- nixosModules flake
  services <- nixosServices flake hostname
  programs <- nixosPrograms flake hostname
  packages <- nixosPackages flake hostname
  let matchModules = filter (L.isInfixOf query) modules
      matchServices = Map.keys $ Map.filterWithKey (\k _ -> L.isInfixOf query k) services
      matchPrograms = Map.keys $ Map.filterWithKey (\k _ -> L.isInfixOf query k) programs
      matchPackages = filter (L.isInfixOf query) packages
  let results = map (, "module") matchModules
        ++ map (, "service") matchServices
        ++ map (, "program") matchPrograms
        ++ map (, "package") matchPackages
  if null results
  then do
    logInfo "No matching results found."
  else do
    forM_ results $ \(name, kind) -> do
      logInfo $ fromString $ "  " <> name <> " (" <> kind <> ")"

searchUser
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> String -> RIO env ()
searchUser flake username query = do
  modules <- homeModules flake
  services <- homeServices flake username
  programs <- homePrograms flake username
  packages <- homePackages flake username
  let matchModules = filter (L.isInfixOf query) modules
      matchServices = Map.keys $ Map.filterWithKey (\k _ -> L.isInfixOf query k) services
      matchPrograms = Map.keys $ Map.filterWithKey (\k _ -> L.isInfixOf query k) programs
      matchPackages = filter (L.isInfixOf query) packages
  let results = map (, "module") matchModules
        ++ map (, "service") matchServices
        ++ map (, "program") matchPrograms
        ++ map (, "package") matchPackages
  if null results
  then do
    logInfo "No matching results found."
  else do
    forM_ results $ \(name, kind) -> do
      logInfo $ fromString $ "  " <> name <> " (" <> kind <> ")"
