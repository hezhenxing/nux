{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Cmd.OS.Test
  ( testCmd
  ) where

import          Nux.Options
import          Nux.Process
import          Nux.Util
import          RIO

testCmd :: Command (RIO App ())
testCmd = addCommand
  "test"
  "Build and activate NuxOS configuration without making it the boot default"
  (const runTest)
  (pure ())

runTest :: RIO App ()
runTest = do
  flake <- view flakeL >>= followLink
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  logInfo $ fromString $ "Building and activating NuxOS system of host " <> hostname
  flakeTest flake hostname
  logInfo "Successfully activated system!"
