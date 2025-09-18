{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Nux.Pkg where

import           Data.Aeson
import           Nux.Host
import           Nux.Process
import           Nux.User
import           RIO
import qualified RIO.List     as L
import qualified RIO.Map      as Map
import           RIO.Orphans  ()
import           RIO.Process
import           SimplePrompt

nixosModules
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env [String]
nixosModules = flip nixEvalAttrNames "nuxos.nixosModules"

homeModules
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env [String]
homeModules = flip nixEvalAttrNames "nuxos.homeModules"

readAutos
  :: (HasProcessContext env, HasLogFunc env)
  => Bool -> FilePath -> RIO env (Map String (String, String))
readAutos global flakeDir = do
  modules <-
    Map.fromList . map (\n -> (n, ("module", n)))
      <$> if global
            then nixosModules flakeDir
            else homeModules flakeDir
  optsJson <- nixEvalAttr flakeDir
    $ if global then "nuxos.optionsJson" else "nuxos.homeOptionsJson"
  pkgsJson <- nixEvalAttr flakeDir "nuxos.packagesJson"
  options  <- readJson optsJson <&> Map.map ("option",)
  packages <- readJson pkgsJson <&> Map.map ("package",)
  return $ Map.unions [modules, options, packages]
  where
  readJson file = liftIO $
    eitherDecodeFileStrict file >>= \case
      Left err -> throwString err
      Right  r -> return r

readHostAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env (Map String (String, String))
readHostAutos = readAutos True

readUserAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> RIO env (Map String (String, String))
readUserAutos = readAutos False

searchAuto :: String -> Map String (String, String) -> [(String, (String, String))]
searchAuto name autos =
  Map.toList $ Map.filterWithKey go autos
  where
    go k _ = L.isInfixOf name k

addHostAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> [String] -> Bool -> Bool -> RIO env ()
addHostAutos flakeDir hostName names yes switch = do
  let hostFile = userFilePath flakeDir hostName
  host <- readFlakeHost flakeDir hostName
  autos <- readHostAutos flakeDir
  let ns = L.nub names L.\\ hostAutos host
  let
    go (rs, unknowns) name = case Map.lookup name autos of
      Nothing ->
        (rs, name:unknowns)
      Just v ->
        ((name, v):rs, unknowns)
  let (rs, unknowns) = foldl' go ([], []) (L.nub ns)
  when (L.nub names L.\\ ns /= []) $ do
    logInfo "The following have already been installed: "
    forM_ ns $ \n -> do
      logInfo $ fromString $ " " <> n
  if unknowns /= []
    then do
      logInfo "Unknown host packages, options or modules:"
      forM_ unknowns $ \n -> do
        logInfo $ fromString $ "  " <> n
    else if null ns
      then do
        logInfo "No new packages to install"
      else do
        logInfo "Adding the following to host configuration:"
        forM_ rs $ \(name, (kind, attr)) -> do
          logInfo $ fromString $ "  " <> name <> " (" <> kind <> ": " <> attr <> ")"
        unless yes $ do
          y <- yesNo "Do you want to continue the operation"
          unless y $ die "user cancelled operation!"
        writeHost hostFile host { hostAutos = hostAutos host ++ ns}
        when switch $ do
          logInfo "Building and switching NuxOS configuration"
          flakeSwitch flakeDir hostName False
          logInfo "Successfully installed host packages!"

delHostAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> [String] -> RIO env ()
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
  => FilePath -> String -> String -> [String] -> Bool -> Bool -> RIO env ()
addUserAutos flakeDir hostName userName names yes switch = do
  let userFile = userFilePath flakeDir userName
  user <- readFlakeUser flakeDir userName
  autos <- readUserAutos flakeDir
  let ns = L.nub names L.\\ userAutos user
  let
    go (rs, unknowns) name = case Map.lookup name autos of
      Nothing ->
        (rs, name:unknowns)
      Just v ->
        ((name, v):rs, unknowns)
  let (rs, unknowns) = foldl' go ([], []) (L.nub ns)
  when (L.nub names L.\\ ns /= []) $ do
    logInfo "The following have already been installed: "
    forM_ ns $ \n -> do
      logInfo $ fromString $ " " <> n
  if unknowns /= []
    then do
      logInfo "Unknown home packages, options or modules:"
      forM_ unknowns $ \n -> do
        logInfo $ fromString $ "  " <> n
    else if null ns
      then do
        logInfo "No new packages to install"
      else do
        logInfo "Adding the following to home configuration:"
        forM_ rs $ \(name, (kind, attr)) -> do
          logInfo $ fromString $ "  " <> name <> " (" <> kind <> ": " <> attr <> ")"
        unless yes $ do
          y <- yesNo "Do you want to continue the operation"
          unless y $ die "user cancelled operation!"
        writeUser userFile user { userAutos = userAutos user ++ ns}
        when switch $ do
          logInfo "Building and switching NuxOS configuration"
          flakeSwitch flakeDir hostName False
          logInfo "Successfully installed user packages!"

delUserAutos
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> [String] -> RIO env ()
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

searchHost :: (HasProcessContext env, HasLogFunc env) => FilePath -> String -> RIO env ()
searchHost flake query = do
  autos <- readHostAutos flake
  let results = searchAuto query autos
  if null results
  then do
    logInfo "No matching results found."
  else do
    forM_ results $ \(name, (kind, attr)) -> do
      logInfo $ fromString $ "  " <> name <> " (" <> kind <> ": " <> attr <> ")"

searchUser :: (HasProcessContext env, HasLogFunc env) => FilePath -> String -> RIO env ()
searchUser flake query = do
  autos <- readUserAutos flake
  let results = searchAuto query autos
  if null results
  then do
    logInfo "No matching results found."
  else do
    forM_ results $ \(name, (kind, attr)) -> do
      logInfo $ fromString $ "  " <> name <> " (" <> kind <> ": " <> attr <> ")"
