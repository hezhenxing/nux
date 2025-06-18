{-# LANGUAGE NoImplicitPrelude #-}
module Nux.Cmd
  ( nux
  ) where

import RIO
import Nux.Options
import Nux.Cmd.Say
import Nux.Cmd.OS
import Nux.Cmd.User
import Nux.Cmd.PM
import Nux.Cmd.Host
import Nux.Cmd.FS

nux :: Command (RIO App ())
nux = do
  sayCmds
  osCmds
  userCmds
  pmCmds
  hostCmds
  fsCmds
