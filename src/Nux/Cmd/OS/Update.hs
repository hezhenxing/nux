{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Update
  ( updateCmd
  ) where

import           Nux.Options
import           Nux.OS
import           Nux.Util
import           RIO

updateCmd :: Command (RIO App ())
updateCmd = addCommand
  "update"
  "Update flake lock file"
  (const runUpdate)
  (pure ())

runUpdate :: RIO App ()
runUpdate = do
  flake <- view flakeL
  hostname <- view hostL
  logInfo $ fromString $ "Updating flake in " <> flake
  void $ nixFlake "update" ["--flake", flake]
  logInfo "Building and switching to updated system"
  nixosSwitchFlake flake hostname
  logInfo $ fromString $ "Successfully updated flake in " <> flake <> "!"
