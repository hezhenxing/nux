{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.OS.VM
  ( vmCmd
  ) where

import RIO
import Nux.Options
import Nux.Util

vmCmd :: Command (RIO App ())
vmCmd = addCommand
  "vm"
  "Build and run virtual machine from the NuxOS configuration"
  (const runvm)
  (pure ())

runvm :: RIO App ()
runvm = do
  flake <- view flakeL
  logInfo $ "Building NuxOS VM from flake: " <> fromString flake
  tryAny (nixrun [flake <> "#nixosConfigurations.nuxos.config.system.build.vm"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to vm NuxOS: " <> displayException e
    Right _ -> do
      logInfo "vmed to NuxOS configuration successfully."
