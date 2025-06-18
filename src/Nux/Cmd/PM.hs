{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.PM
  ( pmCmds
  ) where

import RIO
import RIO.FilePath
import qualified RIO.Map as Map
import Nux.Host
import Nux.Options

pmCmds :: Command (RIO App ())
pmCmds = addSubCommands
  "pm"
  "Package manager commands"
  (do addCmd
      delCmd
      listCmd
  )

data AddOptions = AddOptions
  { addOptNames   :: [String]
  , addOptGlobal  :: Bool
  -- , addOptModule  :: Bool
  -- , addOptService :: Bool
  -- , addOptProgram :: Bool
  -- , addOptPackage :: Bool
  } deriving (Show, Eq)

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add package to Nux system"
  runAdd
  (AddOptions <$> some (strArgument ( metavar "NAME"
                                     <> help "Package name to be installed"
                                     ))
              <*> switch ( long "global"
                        <> short 'g'
                        <> help "Add package globally (system-wide) instead of user-specific"
                          )
              -- <*> switch ( long "module"
              --           <> short 'm'
              --           <> help "Add package as a module"
              --           )
              -- <*> switch ( long "service"
              --           <> short 's'
              --           <> help "Add package as a service"
              --           )
              -- <*> switch ( long "program"
              --           <> short 'p'
              --           <> help "Add package as a program"
              --           )
              -- <*> switch ( long "package"
              --           <> short 'P'
              --           <> help "Add normal packages"
              --           )
  )

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  let hostsDir = flake </> "nix/hosts"
  let hostDir = hostsDir </> hostname
  let hostFile = hostDir </> "host.json"
  host <- readHost hostFile
  logInfo $ fromString $ "Adding " <> title addOptGlobal <> " to host " <> hostname
  writeHost hostFile $ foldl' (go username) host addOptNames
  logInfo $ fromString $ "Successfully added " <> title addOptGlobal <> "!"
  where
    go username host name =
      if addOptGlobal
        then addHostAuto name host
        else addUserAuto name username host

data DelOptions = DelOptions
  { delOptNames   :: [String]
  , delOptGlobal  :: Bool
  } deriving (Show, Eq)

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete package from Nux system"
  runDel
  (DelOptions <$> some (strArgument ( metavar "NAME"
                                     <> help "Package name to be deleted"
                                     ))
              <*> switch ( long "global"
                        <> short 'g'
                        <> help "Delete package globally (system-wide) instead of user-specific"
                          )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  let hostsDir = flake </> "nix/hosts"
  let hostDir = hostsDir </> hostname
  let hostFile = hostDir </> "host.json"
  host <- readHost hostFile
  logInfo $ fromString $ "Deleting " <> title delOptGlobal <> " from host " <> hostname
  writeHost hostFile $ foldl' (go username) host delOptNames
  logInfo $ fromString $ "Successfully deleted " <> title delOptGlobal <> "!"
  where
    go username host name =
      if delOptGlobal
        then delHostAuto name host
        else delUserAuto name username host


data ListOptions = ListOptions
  { listOptGlobal :: Bool
  , listOptAll    :: Bool
  } deriving (Show, Eq)

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List packages in Nux system"
  runList
  (ListOptions <$> switch ( long "global"
                          <> short 'g'
                          <> help "List global (system-wide) packages instead of user-specific"
                            )
              <*> switch ( long "all"
                          <> short 'a'
                          <> help "List all packages, both user-specific and global"
                            )
  )

runList :: ListOptions -> RIO App ()
runList ListOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  let hostsDir = flake </> "nix/hosts"
  let hostDir = hostsDir </> hostname
  let hostFile = hostDir </> "host.json"
  host <- readHost hostFile
  when (listOptGlobal || listOptAll) $ do
    logInfo "Global packages"
    forM_ (hostAutos host) $ \name -> do
      logInfo $ fromString name
  when (not listOptGlobal || listOptAll) $ do
    logInfo "User pacakges"
    case Map.lookup username (hostUsers host) of
      Nothing -> throwString $ "user not found: " <> username
      Just user -> forM_ (userAutos user) $ \name -> do
        logInfo $ fromString name

title :: Bool -> String
title global = if global
  then "global packages"
  else "user packages"
