{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.VM
  ( vmCmd
  ) where

import           Nux.Options
import           Nux.Util
import           RIO

vmCmd :: Command (RIO App ())
vmCmd = addCommand
  "vm"
  "Build and run virtual machine from the NuxOS configuration"
  (const runVM)
  (pure ())

runVM :: RIO App ()
runVM = do
  flake <- view flakeL
  hostname <- view hostL
  logInfo $ fromString $ "Building and running NuxOS VM of " <> hostname <> " from flake " <> flake
  tryAny (nixrun [flake <> "#nixosConfigurations." <> hostname <> ".config.system.build.vm"]) >>= \case
    Left e -> do
      logError $ fromString $ "Failed to run vm: " <> displayException e
    Right _ -> do
      logInfo "Run VM successfully."
