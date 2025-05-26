{-# LANGUAGE NoImplicitPrelude #-}
module Nux.Cmd
  ( nux
  ) where

import RIO
import Nux.Options
import Nux.Cmd.Say
import Nux.Cmd.OS
import Nux.Cmd.User

nux :: Command (RIO App ())
nux = do
  sayCmds
  osCmds
  userCmds
