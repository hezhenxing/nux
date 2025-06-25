{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Nux.Options
  ( module Options.Applicative.Simple
  , Options(..)
  , App(..)
  , HasFlake(..)
  , HasForce(..)
  , HasSystem(..)
  , HasHost(..)
  , HasUser(..)
  , Command
  , addSubCommandsWithOptions
  ) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Writer
import           Options.Applicative.Simple
import           RIO
import           RIO.Process

data Options = Options
  { optVerbose :: Bool
  , optForce   :: Bool
  , optFlake   :: FilePath
  , optSystem  :: String
  , optHost    :: String
  , optUser    :: String
  } deriving (Show, Eq)

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions        :: !Options
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

class HasForce env where
  forceL :: Lens' env Bool
instance HasForce App where
  forceL = lens (optForce . appOptions)
    (\x y -> x { appOptions = (appOptions x) { optForce = y } })

class HasSystem env where
  systemL :: Lens' env String
instance HasSystem App where
  systemL = lens (optSystem . appOptions)
    (\x y -> x { appOptions = (appOptions x) { optSystem = y } })

class HasHost env where
  hostL :: Lens' env String
instance HasHost App where
  hostL = lens (optHost . appOptions)
    (\x y -> x { appOptions = (appOptions x) { optHost = y } })

class HasUser env where
  userL :: Lens' env String
instance HasUser App where
  userL = lens (optUser . appOptions)
    (\x y -> x { appOptions = (appOptions x) { optUser = y } })

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
