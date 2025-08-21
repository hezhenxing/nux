{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.Update
  ( updateCmd
  ) where

import           Nux.Options
import           Nux.Process
import           Nux.Util
import           RIO
import           SimplePrompt

updateCmd :: Command (RIO App ())
updateCmd = addCommand
  "update"
  "Update flake lock file"
  (const runUpdate)
  (pure ())

runUpdate :: RIO App ()
runUpdate = do
  yes <- view yesL
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Updating NuxOS configuration in " <> flake
  flakeUpdate flake
  logInfo "Updated the configuration!"
  unless yes $ do
    y <- yesNo "Do you want to upgrade the system"
    unless y $ die "user cancelled upgrade"
  logInfo "Upgrading the system"
  flakeSwitch flake hostname
  logInfo "Successfully upgraded system!"
