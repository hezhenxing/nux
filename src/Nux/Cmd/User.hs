{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.User
  ( userCmds
  ) where

import RIO
import RIO.FilePath
import qualified RIO.Map as Map
import Nux.Host
import Nux.Options
import Nux.Util

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
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  case Map.lookup addOptUsername (hostUsers host) of
    Just _ -> do
      throwString $ "User already exists: " <> addOptUsername
    Nothing -> do
      let user = emptyUser { userDescription = addOptDescription
                           , userEmail       = addOptEmail
                           , userUid         = if addOptUid == 0 then Nothing else Just addOptUid
                           , userGid         = if addOptGid == 0 then Nothing else Just addOptGid
                           }
      writeHost hostFile $ host { hostUsers = Map.insert addOptUsername user (hostUsers host)}
      logInfo $ fromString $ "Successfully added user " <> addOptUsername <> "!"

data DelOptions = DelOptions
  { delOptUsername :: String
  } deriving (Show, Eq)

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete an existing user"
  runDel
  (DelOptions <$> strArgument ( metavar "USERNAME"
                                <> help "Username to be deleted"
                                 )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  case Map.lookup delOptUsername (hostUsers host) of
    Nothing -> do
      throwString $ "User not exists: " <> delOptUsername
    Just _ -> do
      writeHost hostFile $ host { hostUsers = Map.delete delOptUsername (hostUsers host)}
      logInfo $ fromString $ "Successfully deleted user " <> delOptUsername <> "!"

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List existing users"
  (const runList)
  (pure ())

runList :: RIO App ()
runList = do
  flake <- view flakeL
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  logInfo $ fromString $ "Users in host " <> hostname
  mapM_ (logInfo . fromString) (Map.keys (hostUsers host))

data EditOptions = EditOptions
  { editOptNewUid :: Int
  , editOptNewGid :: Int
  , editOptNewDescription :: String
  , editOptNewEmail :: String
  , editOptUser :: String
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
  hostname <- view hostL
  let hostFile = flake </> "nix/hosts" </> hostname </> "host.json"
  host <- readHost hostFile
  case Map.lookup editOptUser (hostUsers host) of
    Nothing -> throwString $ "User not found: " <> editOptUser
    Just u -> do
      let user = u { userUid = editOptNewUid `zeroOrJust` userUid u
                   , userGid = editOptNewGid `zeroOrJust` userGid u
                   , userDescription = editOptNewDescription `nullOr` userDescription u
                   , userEmail = editOptNewEmail `nullOr` userEmail u
                   }
      writeHost hostFile $ host { hostUsers = Map.insert editOptUser user (hostUsers host) }
      logInfo $ fromString $ "Successfully updated user " <> editOptUser <> "!"
  where
    zeroOrJust a b = if a == 0 then b else Just a
