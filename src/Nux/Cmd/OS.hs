{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS
  ( osCmds
  ) where

import           Nux.Cmd.OS.Init
import           Nux.Cmd.OS.Install
import           Nux.Cmd.OS.List
import           Nux.Cmd.OS.Rollback
import           Nux.Cmd.OS.Switch
import           Nux.Cmd.OS.Test
import           Nux.Cmd.OS.Update
import           Nux.Cmd.OS.VM
import           Nux.Options
import           Nux.Process
import           Nux.Util
import           RIO

osCmds :: Command (RIO App ())
osCmds = addSubCommands
  "os"
  "OS commands"
  (do initCmd
      installCmd
      rollbackCmd
      updateCmd
      switchCmd
      vmCmd
      listCmd
      testCmd
      bootCmd
  )

bootCmd :: Command (RIO App ())
bootCmd = addCommand
  "boot"
  "Make the current system the boot default"
  (const runBoot)
  (pure ())

runBoot :: RIO App ()
runBoot = do
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  logInfo $ fromString $ "Building and activating NuxOS system of host " <> hostname
  run $ cmd "/run/current-system/bin/switch-to-configuration"
      & arg "boot"
      & sudo
  logInfo "Successfully activated system!"
