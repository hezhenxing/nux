{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.Pkg
  ( pkgCmds
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.Pkg
import           Nux.Process
import           Nux.User
import           RIO

pkgCmds :: Command (RIO App ())
pkgCmds = addSubCommands
  "pkg"
  "Package manager commands"
  (do addCmd
      delCmd
      listCmd
      searchCmd
      installCmd
      removeCmd
  )

data InstallOptions = InstallOptions
  { installOptGlobal :: Bool
  , installOptNames  :: [String]
  }

installCmd :: Command (RIO App ())
installCmd = addCommand
  "install"
  "Add packages and switch to new NuxOS configuration"
  runInstall
  (InstallOptions
    <$> switch ( long "global"
              <> short 'g'
              <> help "Add package globally (system-wide) instead of user-specific"
               )
    <*> some (strArgument ( metavar "PACKAGE"
                         <> help "Package name to be installed"
                          ))
  )

runInstall :: InstallOptions -> RIO App ()
runInstall InstallOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  logInfo $ fromString $ "Using NuxOS in " <> flake
  if installOptGlobal
    then do
      logInfo $ fromString $ "Adding system packages to host " <> hostname
      addHostAutos flake hostname installOptNames
    else do
      logInfo $ fromString $ "Adding user packages to user " <> username
      addUserAutos flake username installOptNames
  logInfo "Building and switching NuxOS configuration"
  flakeSwitch flake hostname
  logInfo "Successfully installed system packages!"

data RemoveOptions = RemoveOptions
  { removeOptGlobal :: Bool
  , removeOptNames  :: [String]
  }

removeCmd :: Command (RIO App ())
removeCmd = addCommand
  "remove"
  "Remove packages from Nux system"
  runRemove
  (RemoveOptions
    <$> switch ( long "global"
              <> short 'g'
              <> help "Remove package globally (system-wide) instead of user-specific"
               )
    <*> some (strArgument ( metavar "PACKAGE"
                         <> help "Package name to be removed"
                          ))
  )

runRemove :: RemoveOptions -> RIO App ()
runRemove RemoveOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  logInfo $ fromString $ "Using NuxOS in " <> flake
  if removeOptGlobal
    then do
      logInfo $ fromString $ "Deleting system packages from host " <> hostname
      delHostAutos flake hostname removeOptNames
    else do
      logInfo $ fromString $ "Deleting user packages from user " <> username
      delUserAutos flake username removeOptNames
  logInfo "Building and switching NuxOS configuration"
  flakeSwitch flake hostname
  logInfo "Successfully installed system packages!"

data AddOptions = AddOptions
  { addOptNames  :: [String]
  , addOptGlobal :: Bool
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
  )

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  if addOptGlobal
  then do
    logInfo $ fromString $ "Adding packages to host " <> hostname
    addHostAutos flake hostname addOptNames
    logInfo $ fromString $ "Successfully added host packages!"
  else do
    logInfo $ fromString $ "Adding packages to user " <> username
    addUserAutos flake username addOptNames
    logInfo $ fromString $ "Successfully added user packages!"

data DelOptions = DelOptions
  { delOptNames  :: [String]
  , delOptGlobal :: Bool
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
  if delOptGlobal
  then do
    logInfo $ fromString $ "Deleting packages from host " <> hostname
    delHostAutos flake hostname delOptNames
    logInfo "Successfully deleted host packages!"
  else do
    logInfo $ fromString $ "Deleting packages from user " <> username
    delUserAutos flake username delOptNames
    logInfo "Successfully deleted user packages!"

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
  when (listOptGlobal || listOptAll) $ do
    logInfo "Global packages"
    exists <- doesHostExist flake hostname
    if exists
    then
      listHostAutos flake hostname
    else
      throwString $ "Host not found: " <> hostname
  when (not listOptGlobal || listOptAll) $ do
    logInfo "User pacakges"
    exists <- doesUserExist flake username
    if exists
    then
      listUserAutos flake username
    else
      throwString $ "User not found: " <> username

data SearchOptions = SearchOptions
  { searchOptGlobal :: Bool
  , searchOptQuery  :: String
  } deriving (Show, Eq)

searchCmd :: Command (RIO App ())
searchCmd = addCommand
  "search"
  "Search packages in Nux system"
  runSearch
  (SearchOptions
    <$> switch ( long "global"
              <> short 'g'
              <> help "Search global (system-wide) packages instead of user-specific"
               )
    <*> strArgument ( metavar "QUERY"
                   <> help "Query string to search for packages"
                    )
  )

runSearch :: SearchOptions -> RIO App ()
runSearch SearchOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  username <- view userL
  logInfo $ fromString $ "Searching in flake " <> flake
  if searchOptGlobal
    then do
      logInfo $ fromString $ "Searching system packages of host " <> hostname
      exists <- doesHostExist flake hostname
      if exists
        then do
          searchHost flake hostname searchOptQuery
      else do
        logError $ fromString $ "Host not found: " <> hostname
  else do
    logInfo $ fromString $ "Searching user packages of user " <> username
    existsUser <- doesUserExist flake username
    if existsUser
      then do
        searchUser flake username searchOptQuery
      else do
        logError $ fromString $ "User not found: " <> username
