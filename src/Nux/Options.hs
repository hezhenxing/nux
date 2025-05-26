{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Nux.Options
  ( module Options.Applicative.Simple
  , Options(..)
  , App(..)
  , HasFlake(..)
  , Command
  , addSubCommandsWithOptions
  ) where

import RIO
import RIO.Process
import Options.Applicative.Simple
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer

data Options = Options
  { optVerbose :: Bool
  , optFlake   :: FilePath
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

class HasFlake env where
  flakeL :: Lens' env FilePath
instance HasFlake App where
  flakeL = lens (optFlake . appOptions)
    (\x y -> x { appOptions = (appOptions x) { optFlake = y } })

type Command a =
  ExceptT a (Writer (Mod CommandFields a)) ()

-- | Like addSubCommands, but allow common options for all sub-commands.
addSubCommandsWithOptions
  :: String
  -> String
  -> Parser a
  -> Command (a -> b)
  -> Command b
addSubCommandsWithOptions cmd title optsParser commandParser =
  addCommand
    cmd
    title
    (\(opts, runSubCmd) -> runSubCmd opts)
    (simpleParser optsParser commandParser)
