{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.VM
  ( vmCmd
  ) where

import           Nux.Options
import           Nux.Process
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
  logInfo $ fromString $ "Using NuxOS configuration " <> flake
  logInfo $ fromString $ "Building and running vm of host " <> hostname
  rc <- runExitCode
    $ cmd "nix"
    & arg "run"
    & arg (flake <> "#nixosConfigurations." <> hostname <> ".config.system.build.vm")
  case rc of
    ExitFailure ec -> do
      logError $ fromString $ "Running vm failed with exit code: " <> show ec
    ExitSuccess -> do
      logInfo "Run vm successfully."
