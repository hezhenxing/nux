{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.OS.Switch
  ( switchCmd
  ) where

import RIO
import Nux.Options
import Nux.Util

switchCmd :: Command (RIO App ())
switchCmd = addCommand
  "switch"
  "Build and switch to configuration"
  (const runSwitch)
  (pure ())

runSwitch :: RIO App ()
runSwitch =
  tryAny (nixosSwitch ["--flake", "./nuxos#nux"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to switch NuxOS: " <> displayException e
    Right _ -> do
      logInfo "Switched to NuxOS configuration successfully."
