{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.OS.Switch
  ( switchCmd
  ) where

import RIO
import Nux.Options
import System.Process (system)

switchCmd :: Command (RIO App ())
switchCmd = addCommand
  "switch"
  "Build and switch to configuration"
  (const runSwitch)
  (pure ())

runSwitch :: RIO App ()
runSwitch = do
  (liftIO $ system "sudo nixos-rebuild switch --flake ./nuxos#nux") >>= \case
    ExitSuccess ->
      return ()
    ExitFailure rc ->
      logError $ fromString $ "nixos-rebuild switch command failed with exist code: " <> show rc

