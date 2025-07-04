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
  flake <- view flakeL
  hostname <- view hostL
  logInfo $ fromString $ "Updating flake in " <> flake
  void $ nixFlake "update" ["--flake", flake]
  logInfo "Updated the configuration!"
  unless updateOptYes $ do
    yes <- yesNo "Do you want to continue upgrading the system?"
    unless yes $ throwString "user cancelled upgrade"
  logInfo "Upgrading the system"
  nixosSwitchFlake flake hostname
  logInfo "Successfully upgraded system!"
