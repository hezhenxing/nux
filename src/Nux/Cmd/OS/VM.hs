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
  hostname <- view hostL
  logInfo $ fromString $ "Building NuxOS VM of " <> hostname <> " from flake " <> flake
  tryAny (nixrun [flake <> "#nixosConfigurations." <> hostname <> ".config.system.build.vm"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to start vm: " <> displayException e
    Right _ -> do
      logInfo "Start VM of NuxOS configuration successfully."
