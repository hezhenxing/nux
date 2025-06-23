{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.PM
  ( pmCmds
  ) where

import RIO
import RIO.Directory
import RIO.FilePath
import qualified RIO.Map as Map
import Nux.Host
import Nux.Options
import Nux.User

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
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  let userFile = flake </> "nix/users" </> username </> "user.json"
  logInfo $ fromString $ "Adding " <> title addOptGlobal <> " to host " <> hostname
  if addOptGlobal
  then do
    host <- readHost hostFile
    writeHost hostFile $ foldl' (flip addHostAuto) host addOptNames
  else do
    user <- readUser userFile
    writeUser userFile $ foldl' (flip addUserAuto) user addOptNames
  logInfo $ fromString $ "Successfully added " <> title addOptGlobal <> "!"

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
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  let userFile = flake </> "nix/users" </> username </> "user.json"
  logInfo $ fromString $ "Deleting " <> title delOptGlobal <> " from host " <> hostname
  if delOptGlobal
  then do
    host <- readHost hostFile
    writeHost hostFile $ foldl' (flip delHostAuto) host delOptNames
  else do
    user <- readUser userFile
    writeUser userFile $ foldl' (flip delUserAuto) user delOptNames
  logInfo $ fromString $ "Successfully deleted " <> title delOptGlobal <> "!"

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
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  let userFile = flake </> "nix/users" </> username </> "user.json"
  host <- readHost hostFile
  when (listOptGlobal || listOptAll) $ do
    logInfo "Global packages"
    forM_ (hostAutos host) $ \name -> do
      logInfo $ fromString $ "  " <> name
  when (not listOptGlobal || listOptAll) $ do
    logInfo "User pacakges"
    exists <- doesFileExist userFile
    if exists
    then do
      user <- readUser userFile
      forM_ (userAutos user) $ \name -> do
        logInfo $ fromString $ "  " <> name
    else
      throwString $ "User not found: " <> username

title :: Bool -> String
title global = if global
  then "global packages"
  else "user packages"
