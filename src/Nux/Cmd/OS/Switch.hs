{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.Switch
  ( switchCmd
  ) where

import           Nux.Options
import           Nux.OS
import           Nux.Util
import           RIO

switchCmd :: Command (RIO App ())
switchCmd = addCommand
  "switch"
  "Build and switch system to NuxOS configuration"
  (const runSwitch)
  (pure ())

runSwitch :: RIO App ()
runSwitch = do
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  logInfo $ fromString $ "Building and switching NuxOS system of host " <> hostname
  nixosSwitchFlake flake hostname
  logInfo "Successfully upgraded system!"
