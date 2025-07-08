{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Update
  ( updateCmd
  ) where

import           Nux.Options
import           Nux.Process
import           Nux.Util
import           RIO
import           SimplePrompt

data UpdateOptions = UpdateOptions
  { updateOptYes :: Bool
  }

updateCmd :: Command (RIO App ())
updateCmd = addCommand
  "update"
  "Update flake lock file"
  runUpdate
  (UpdateOptions
    <$> switch ( long "yes"
              <> short 'y'
              <> help "Assume yes for all confirmation prompts"
               )
  )

runUpdate :: UpdateOptions -> RIO App ()
runUpdate UpdateOptions{..} = do
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Updating NuxOS configuration in " <> flake
  flakeUpdate flake
  logInfo "Updated the configuration!"
  unless updateOptYes $ do
    yes <- yesNo "Do you want to upgrade the system"
    unless yes $ die "user cancelled upgrade"
  logInfo "Upgrading the system"
  flakeSwitch flake hostname
  logInfo "Successfully upgraded system!"
