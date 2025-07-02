{-# LANGUAGE NoImplicitPrelude #-}
module Nux.Cmd
  ( nux
  ) where

import           Nux.Cmd.FS
import           Nux.Cmd.Host
import           Nux.Cmd.OS
import           Nux.Cmd.Pkg
import           Nux.Cmd.Say
import           Nux.Cmd.User
import           Nux.Options
import           RIO

nux :: Command (RIO App ())
nux = do
  sayCmds
  osCmds
  userCmds
  pkgCmds
  hostCmds
  fsCmds
