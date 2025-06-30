{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.Init
  ( initCmd,
  ) where

import           Nux.Options
import           Nux.OS
import           Nux.Util
import           RIO
import           RIO.Directory

initCmd :: Command (RIO App ())
initCmd = addCommand
  "init"
  "Initialize Nux in the current directory"
  (const runInit)
  (pure ())

runInit :: RIO App ()
runInit = do
  flake <- view flakeL
  isForce <- view forceL
  url <- view urlL
  logInfo $ fromString $ "Starting Nux initialization in " <> flake
  createDirectoryIfMissing True flake
  isEmpty <- isDirectoryEmpty flake
  unless isEmpty $ do
    if isForce
      then do
        logWarn "Directory is not empty, Forcing overwrite existing files..."
      else do
        logError "The target directory is not empty."
        throwString $ "directory not empty: " <> flake
  logInfo $ fromString $ "Initializing Nux in directory " <> flake
  initFlake flake url
  logInfo "Nux initialized successfully."
