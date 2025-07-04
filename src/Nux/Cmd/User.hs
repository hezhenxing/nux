{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.User
  ( userCmds
  ) where

import           Nux.Options
import           Nux.User
import           Nux.Util
import           RIO
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List      as L

userCmds :: Command (RIO App ())
userCmds = addSubCommands
  "user"
  "User commands"
  (do addCmd
      delCmd
      listCmd
      editCmd
  )

data AddOptions = AddOptions
  { addOptUsername    :: String
  , addOptDescription :: String
  , addOptEmail       :: String
  , addOptUid         :: Int
  , addOptGid         :: Int
  } deriving (Show, Eq)

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add a new user"
  runAdd
  (AddOptions <$> strArgument ( metavar "USERNAME"
                             <> help "Username to be added"
                              )
              <*> strOption ( long "description"
                            <> short 'd'
                            <> value ""
                            <> help "Description of the user"
                            )
              <*> strOption ( long "email"
                            <> short 'e'
                            <> value ""
                            <> help "Email of the user"
                            )
              <*> option auto ( long "uid"
                             <> short 'u'
                             <> help "ID number of the user"
                             <> value 0
                             )
              <*> option auto ( long "gid"
                             <> short 'g'
                             <> help "ID number of the group"
                             <> value 0
                             )
  )

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  isForce <- view forceL
  logInfo $ fromString $ "Using NixOS configuration in " <> flake
  let userDir = flake </> "nix/users" </> addOptUsername
  let nixFile = userDir </> "default.nix"
  let jsonFile = userDir </> "user.json"
  exists <- doesPathExist nixFile
  if exists && not isForce
  then
    throwString $ "User already exists: " <> addOptUsername
  else do
    let user = emptyUser { userDescription = addOptDescription
                         , userEmail       = addOptEmail
                         , userUid         = addOptUid
                         , userGid         = addOptGid
                         }
    createDirectoryIfMissing True userDir
    writeBinaryFile nixFile $ fromString $ L.unlines
      [ "with builtins; fromJSON (readFile ./user.json)"
      ]
    writeUser jsonFile user
    logInfo $ fromString $ "Successfully added user " <> addOptUsername <> "!"

data DelOptions = DelOptions
  { delOptUsername        :: String
  , delOptRemoveDirectory :: Bool
  }

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete an existing user"
  runDel
  (DelOptions
    <$> strArgument ( metavar "USERNAME"
                   <> help "Username to be deleted"
                    )
    <*> switch ( long "remove-directory"
              <> short 'R'
              <> help "Remove user directory, normally only user config files are deleted"
               )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  logInfo $ fromString $ "Using NixOS configuration in " <> flake
  let userDir = flake </> "nix/users" </> delOptUsername
  let nixFile = userDir </> "default.nix"
  let jsonFile = userDir </> "user.json"
  exists <- doesDirectoryExist userDir
  if exists
  then do
    if delOptRemoveDirectory
      then do
        removeDirectoryRecursive userDir
      else do
        removeFile nixFile
        removeFile jsonFile
        isempty <- isDirectoryEmpty userDir
        if isempty
          then do
            removeDirectory userDir
          else do
            logWarn "User directory is not empty after delete config files, user directory not removed"
    logInfo $ fromString $ "Successfully deleted user " <> delOptUsername <> "!"
  else do
    throwString $ "User not found: " <> delOptUsername

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List existing users"
  (const runList)
  (pure ())

runList :: RIO App ()
runList = do
  flake <- view flakeL
  let usersDir = flake </> "nix/users"
  names <- listDirectory usersDir
  logInfo "Users in configuration"
  forM_ names $ \name -> do
    let nixFile = usersDir </> name </> "default.nix"
    exists <- doesFileExist nixFile
    when exists $
      logInfo $ fromString name

data EditOptions = EditOptions
  { editOptNewUid         :: Int
  , editOptNewGid         :: Int
  , editOptNewDescription :: String
  , editOptNewEmail       :: String
  , editOptUser           :: String
  }

editCmd :: Command (RIO App ())
editCmd = addCommand
  "edit"
  "Edit existing user"
  runEdit
  (EditOptions
    <$> option auto
      (  long "uid"
      <> short 'u'
      <> help "New user id"
      <> value 0
      )
    <*> option auto
      (  long "gid"
      <> short 'g'
      <> help "New group id"
      <> value 0
      )
    <*> strOption
      (  long "description"
      <> short 'd'
      <> help "New user description"
      <> value ""
      )
    <*> strOption
      (  long "email"
      <> short 'e'
      <> help "New user email address"
      <> value ""
      )
    <*> strArgument
      (  metavar "USER"
      <> help "User name to edit"
      )
  )

runEdit :: EditOptions -> RIO App ()
runEdit EditOptions{..} = do
  flake <- view flakeL
  logInfo $ fromString $ "Using NixOS configuration in " <> flake
  let jsonFile = flake </> "nix/users" </> editOptUser </> "user.json"
  exists <- doesFileExist jsonFile
  if exists
  then do
    u <- readUser jsonFile
    let user = u { userUid = editOptNewUid `zeroOr` userUid u
                 , userGid = editOptNewGid `zeroOr` userGid u
                 , userDescription = editOptNewDescription `nullOr` userDescription u
                 , userEmail = editOptNewEmail `nullOr` userEmail u
                 }
    writeUser jsonFile user
    logInfo $ fromString $ "Successfully updated user " <> editOptUser <> "!"
  else
    throwString $ "User not found: " <> editOptUser
