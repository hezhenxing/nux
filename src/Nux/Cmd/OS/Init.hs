{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Cmd.OS.Init
  ( initCmd,
  ) where

import RIO
import RIO.Directory
import Nux.Expr
import Nux.Flake
import Nux.Options

data InitOptions = InitOptions
  { initOptForce :: Bool
  } deriving (Show, Eq)

initCmd :: Command (RIO App ())
initCmd = addCommand
  "init"
  "Initialize Nux in the current directory"
  run
  opts

opts :: Parser InitOptions
opts = InitOptions
  <$> switch ( long "force"
             <> short 'f'
             <> help "Force initialization"
             )

run :: InitOptions -> RIO App ()
run InitOptions{..} = do
  dir <- getCurrentDirectory
  isEmpty <- isEmptyDirectory dir
  unless isEmpty $ do
    if initOptForce
      then logWarn "Directory is not empty, Forcing overwrting existing files..."
      else do
        logError "The directory is not empty. Please run this command in an empty directory."
        throwString $ "directory not empty: " <> dir
  logInfo "Initializing Nux..."

isEmptyDirectory :: HasLogFunc env => FilePath -> RIO env Bool
isEmptyDirectory dir = do
  isDir <- doesDirectoryExist dir
  unless isDir $ do
    logError "The specified path is not a directory."
    throwString $ "not a directory: " <> dir
  contents <- listDirectory dir
  return $ null contents
