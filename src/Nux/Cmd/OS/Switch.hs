{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Switch
  ( switchCmd
  ) where

import           Nux.Options
import           Nux.Process
import           Nux.Util
import           RIO

data SwitchOptions = SwitchOptions
  { switchOptBootLoader :: Bool
  }

switchCmd :: Command (RIO App ())
switchCmd = addCommand
  "switch"
  "Build and switch system to NuxOS configuration"
  runSwitch
  (SwitchOptions
    <$> switch ( long "bootloader"
              <> short 'b'
              <> help "Install or re-install the boot loader on the device specified by relevant options"
               )
  )

runSwitch :: SwitchOptions -> RIO App ()
runSwitch SwitchOptions{..} = do
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  logInfo $ fromString $ "Building and switching NuxOS system of host " <> hostname
  flakeSwitch flake hostname switchOptBootLoader
  logInfo "Successfully switched system!"
