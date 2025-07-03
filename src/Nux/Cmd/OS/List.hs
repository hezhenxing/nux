module Nux.Cmd.OS.List
  ( listCmd
  ) where

import           Nux.Options
import           Nux.Util
import           RIO

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List NuxOS generations"
  (const runList)
  (pure ())

runList :: RIO App ()
runList = do
  exec "nixos-rebuild" ["list-generations"] >>= logInfo . fromString
