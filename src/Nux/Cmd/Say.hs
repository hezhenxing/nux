{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Cmd.Say
  ( sayCmds
  ) where

import RIO
import Nux.Options

data SayOptions = SayOptions
  { sayOptName :: String
  } deriving (Show, Eq)

hello :: SayOptions -> RIO App ()
hello SayOptions{..} =
  logInfo $ "Hello, " <> (fromString sayOptName) <> "!"

goodbye :: SayOptions -> RIO App ()
goodbye SayOptions{..} =
  logInfo $ "Goodbye, " <> (fromString sayOptName) <> "!"

sayCmds :: Command (RIO App ())
sayCmds = addSubCommandsWithOptions
  "say"
  "Say something"
  (SayOptions
    <$> strOption
        ( long "name"
        <> short 'n'
        <> value "Nux"
        <> help "Name to say to"
        <> metavar "NAME"
        )
  )
  (do addCommand
        "hello"
        "Hello, Nux!"
        (const hello)
        (pure ())
      addCommand
        "goodbye"
        "Goodbye, Nux!"
        (const goodbye)
        (pure ())
  )
