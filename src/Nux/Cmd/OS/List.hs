module Nux.Cmd.OS.List
  ( listCmd
  ) where

import           Nux.Options
import           Nux.Process
import           RIO

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List NuxOS generations"
  (const runList)
  (pure ())

runList :: RIO App ()
runList
  = cmd "nh"
  & arg "os"
  & arg "info"
  & readStdoutString
  >>= logInfo . fromString
