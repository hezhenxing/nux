{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.PM
  ( pmCmds
  ) where

import RIO
import Nux.Host
import Nux.Options
import Nux.User
import Nux.PM

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
  if delOptGlobal
  then do
    logInfo $ fromString $ "Deleting packages from host " <> hostname
    delHostAutos flake hostname delOptNames
    logInfo $ fromString $ "Successfully deleted host packages!"
  else do
    logInfo $ fromString $ "Deleting packages from user " <> username
    delUserAutos flake username delOptNames
    logInfo $ fromString $ "Successfully deleted user packages!"

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
      listHostMSPP flake hostname
    else
      throwString $ "Host not found: " <> hostname
  when (not listOptGlobal || listOptAll) $ do
    logInfo "User pacakges"
    exists <- doesUserExist flake username
    if exists
    then
      listUserMSPP flake username
    else
      throwString $ "User not found: " <> username
