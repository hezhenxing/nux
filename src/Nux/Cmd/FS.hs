{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.FS where

import           Nux.Host
import           Nux.Options
import           Nux.Util
import           RIO
import           RIO.FilePath
import qualified RIO.List     as L
import qualified RIO.Map      as Map

fsCmds :: Command (RIO App ())
fsCmds = addSubCommands
  "fs"
  "File System commands"
  (do addCmd
      delCmd
      listCmd
      editCmd
  )

data AddOptions = AddOptions
  { addOptMountPoint :: String
  , addOptDevice     :: String
  , addOptFsType     :: String
  , addOptOptions    :: String
  }

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add filesystem to configuration"
  runAdd
  (AddOptions <$> strArgument ( metavar "PATH"
                             <> help "Mount point path of the filesystem"
                              )
              <*> strArgument ( metavar "DEVICE"
                             <> help "Device of the filesystem"
                              )
              <*> strArgument ( metavar "FSTYPE"
                             <> help "Type of the filesystem"
                              )
              <*> strOption ( long "options"
                           <> short 'o'
                           <> help "Options of the filesystem"
                           <> value ""
                            )
  )

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  case Map.lookup addOptMountPoint (hostFileSystems host) of
    Just _ -> throwString $ "Filesystem already exist for mount point " <> addOptMountPoint
    Nothing -> do
      let options = splitOptions addOptOptions
      let fs = FileSystem addOptDevice addOptFsType options
      writeHost hostFile $ host { hostFileSystems = Map.insert addOptMountPoint fs (hostFileSystems host)}
      logInfo $ fromString $ "Successfully added filesystem " <> addOptMountPoint <> "!"

data DelOptions = DelOptions
  { delOptMountPoint :: String
  }

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete filesystem from configuration"
  runDel
  (DelOptions <$> strArgument ( metavar "PATH"
                             <> help "Mount point path of the filesystem to be removed"
                              )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  writeHost hostFile $ host { hostFileSystems = Map.delete delOptMountPoint (hostFileSystems host) }
  logInfo $ fromString $ "Successfully deleted filesystem " <> delOptMountPoint <> "!"

data ListOptions = ListOptions
  { listOptAll :: Bool
  }

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List filesystems in configuration"
  runList
  (ListOptions
    <$> switch ( long "all"
              <> short 'a'
              <> help "Show all filesystems, including root and boot filesystems"
               )
  )

runList :: ListOptions -> RIO App ()
runList ListOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  fs <- if listOptAll
    then nixosFileSystems flake hostname
    else do
      let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
      hostFileSystems <$> readHost hostFile
  logInfo
    $ fromString
    $ printTable
    $ hdr : map go (Map.toList fs)
   where
    hdr = ["Filesystem", "Device", "Type", "Options"]
    go (k, f) = [k, fsDevice f, fsType f, L.intercalate "," (fsOptions f)]

data EditOptions = EditOptions
  { editOptNewMountPoint :: String
  , editOptNewDevice     :: String
  , editOptNewType       :: String
  , editOptNewOptions    :: String
  , editOptMountPoint    :: String
  }

editCmd :: Command (RIO App ())
editCmd = addCommand
  "edit"
  "Edit filesystem in configuration"
  runEdit
  (EditOptions
    <$> strOption
      (  long "mountpoint"
      <> short 'm'
      <> help "New mountpoint path of the filesystem"
      <> value ""
      )
    <*> strOption
      (  long "device"
      <> short 'd'
      <> help "New device of the filesystem"
      <> value ""
      )
    <*> strOption
      (  long "type"
      <> short 't'
      <> help "New type of the filesystem"
      <> value ""
      )
    <*> strOption
      (  long "options"
      <> short 'o'
      <> help "New options of the filesystem"
      <> value ""
      )
    <*> strArgument
      (  metavar "PATH"
      <> help "Mountpoint path of the filesystem to edit"
      )
  )

runEdit :: EditOptions -> RIO App ()
runEdit EditOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  case Map.lookup editOptMountPoint (hostFileSystems host) of
    Nothing -> throwString $ "Filesystem not found: " <> editOptMountPoint
    Just fs -> do
      let newOptions = splitOptions editOptNewOptions
      let newfs = fs { fsDevice  = editOptNewDevice `nullOr` fsDevice fs
                     , fsType    = editOptNewType   `nullOr` fsType fs
                     , fsOptions = newOptions       `nullOr` fsOptions fs
                     }
      let newFileSystems = Map.insert editOptNewMountPoint newfs
                         $ Map.delete editOptMountPoint
                         $ hostFileSystems host
      writeHost hostFile $ host { hostFileSystems = newFileSystems }
      logInfo $ fromString $ "Successfully updated filesystem " <> editOptMountPoint <> "!"
