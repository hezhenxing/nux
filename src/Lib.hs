{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
  ( hello
  , goodbye
  , plus
  , Options(..)
  , SayOptions(..)
  , App(..)
  , addSubCommandsWithOptions
  ) where

import RIO
import RIO.Directory
import RIO.Process
import Options.Applicative.Simple
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer

hello :: SayOptions -> RIO App ()
hello SayOptions{..} =
  logInfo $ "Hello, " <> (fromString sayOptName) <> "!"

goodbye :: SayOptions -> RIO App ()
goodbye SayOptions{..} =
  logInfo $ "Goodbye, " <> (fromString sayOptName) <> "!"

plus :: Int -> Int -> Int
plus a b = a + b

data Options = Options
  { optVerbose :: Bool
  } deriving (Show, Eq)

data SayOptions = SayOptions
  { sayOptName :: String
  } deriving (Show, Eq)

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

-- | Like addSubCommands, but allow common options for all sub-commands.
addSubCommandsWithOptions
  :: String
  -> String
  -> Parser a
  -> ExceptT (a -> b) (Writer (Mod CommandFields (a -> b))) ()
  -> ExceptT b (Writer (Mod CommandFields b)) ()
addSubCommandsWithOptions cmd title optsParser commandParser =
  addCommand
    cmd
    title
    (\(opts, runSubCmd) -> runSubCmd opts)
    (simpleParser optsParser commandParser)
