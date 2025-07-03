{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS
  ( osCmds
  ) where

import           Nux.Cmd.OS.Init
import           Nux.Cmd.OS.Install
import           Nux.Cmd.OS.List
import           Nux.Cmd.OS.Rollback
import           Nux.Cmd.OS.Update
import           Nux.Cmd.OS.VM
import           Nux.Options
import           RIO

osCmds :: Command (RIO App ())
osCmds = addSubCommands
  "os"
  "OS commands"
  (do initCmd
      installCmd
      rollbackCmd
      updateCmd
      vmCmd
      listCmd
  )
