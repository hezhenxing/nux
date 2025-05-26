{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.User
  ( userCmds
  ) where

import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import Nux.Expr
import Nux.Options
import Nux.Pretty
import Nux.Util

userCmds :: Command (RIO App ())
userCmds = addSubCommands
  "user"
  "User commands"
  (do addCmd
      delCmd
  )

data AddOptions = AddOptions
  { addOptUsername :: String
  , addOptDescription :: String
  , addOptEmail :: String
  } deriving (Show, Eq)

instance ToNixExpr AddOptions where
  toNixExpr AddOptions{..} = NixAttrs
    [ ("description", NixStr desc)
    , ("email", NixStr email)
    ]
    where
      desc = if null addOptDescription
              then addOptUsername
              else addOptDescription
      email = if null addOptEmail
              then addOptUsername <> "@local"
              else addOptEmail

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
  )

runAdd :: AddOptions -> RIO App ()
runAdd opt@AddOptions{..} = do
  flake <- view flakeL
  let userFile = flake </> "nix/nixosModules/users" </> addOptUsername <> ".nix"
  exist <- doesFileExist userFile
  if exist
    then do
      logError $ fromString $ "User already exists: " <> addOptUsername
      exit 1
    else do
      logInfo $ fromString $ "Writing user file " <> userFile
      writeBinaryFile userFile $ fromString $ showPretty $ toNixExpr opt
      gitC flake "add" ["-f", userFile]
      gitC flake "commit" ["-m", "Add user: " <> addOptUsername]
      logInfo "User added successfully!"

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
  let userFile = flake </> "nix/nixosModules/users" </> delOptUsername <> ".nix"
  exist <- doesFileExist userFile
  if not exist
    then do
      logError $ fromString $ "User does not exist: " <> delOptUsername
      exit 1
    else do
      logInfo $ fromString $ "Removing user file " <> userFile
      removeFile userFile
      gitC flake "rm" [userFile]
      gitC flake "commit" ["-m", "del user: " <> delOptUsername]
      logInfo "User deleted successfully!"
