{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.OS.Pull
  ( pullCmd,
  )
where

import RIO
import Nux.Options
import Nux.Util

pullCmd :: Command (RIO App ())
pullCmd = addCommand
  "pull"
  "Pull NuxOS configuration from remote repository"
  (const runPull)
  (pure ())

runPull :: RIO App ()
runPull = do
  flake <- view flakeL
  logInfo $ fromString $ "Pulling NuxOS configuration to " <> flake
  tryAny (gitC flake "pull" ["--no-rebase", "--no-edit"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to pull NuxOS configuration: " <> displayException e
    Right _ -> do
      logInfo "Pulled NuxOS configuration successfully."
