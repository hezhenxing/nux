{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( hello
  , plus
  , Options(..)
  , App(..)
  ) where

import RIO
import RIO.Process


hello :: RIO App ()
hello = logInfo "Hello, Nux!"

plus :: Int -> Int -> Int
plus a b = a + b

data Options = Options
  { optVerbose :: Bool
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