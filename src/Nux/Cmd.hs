module Nux.Cmd
  ( nux
  ) where

import RIO
import Nux.Options
import Nux.Cmd.Say

nux :: Command (RIO App ())
nux = do
  sayCmds