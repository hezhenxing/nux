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
  { updateOptTest :: Bool
  }

updateCmd :: Command (RIO App ())
updateCmd = addCommand
  "update"
  "Update flake lock file"
  runUpdate
  (UpdateOptions
    <$> switch
      ( long "test"
     <> short 't'
     <> help "Build and activate the update without making it the boot default" )
  )

runUpdate :: UpdateOptions -> RIO App ()
runUpdate UpdateOptions{..} = do
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
  if updateOptTest
    then flakeTest flake hostname
    else flakeSwitch flake hostname
  logInfo "Successfully upgraded system!"
