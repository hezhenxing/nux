{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Cmd.OS.Update
  ( updateCmd
  ) where

import RIO
import Nux.Options
import Nux.Util

updateCmd :: Command (RIO App ())
updateCmd = addCommand
  "update"
  "Update flake lock file"
  (const runUpdate)
  (pure ())

runUpdate :: RIO App ()
runUpdate = do
  flake <- view flakeL
  logInfo $ fromString $ "Updating flake in " <> flake
  nixFlake "update" ["--flake", flake]
  logInfo $ fromString $ "Successfully updated flake in " <> flake <> "!"
