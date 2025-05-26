{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Cmd.OS.Init
  ( initCmd,
  ) where

import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import Nux.Expr
import Nux.Flake
import Nux.Options
import Nux.Util

data InitOptions = InitOptions
  { initOptForce :: Bool
  , initOptDir   :: FilePath
  , initOptUrl   :: String
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
  <*> strArgument ( metavar "DIR"
                 <> value "."
                 <> help "Directory to initialize Nux in"
                  )
  <*> strArgument ( metavar "URL"
                <> help "URL of the NuxOS repository"
                <> value "github:hezhenxing/nuxos"
                )

run :: InitOptions -> RIO App ()
run InitOptions{..} = do
  dir <- makeAbsolute initOptDir
  createDirectoryIfMissing True dir
  isEmpty <- isEmptyDirectory dir
  unless isEmpty $ do
    if initOptForce
      then do
        logWarn "Directory is not empty, Forcing remove existing files..."
        removeDirectoryRecursive dir
      else do
        logError "The target directory is not empty."
        throwString $ "directory not empty: " <> dir
  logInfo $ "Initializing Nux in directory: " <> fromString dir
  git "clone" [initOptUrl, dir]
  logInfo "Nux initialized successfully."

isEmptyDirectory :: HasLogFunc env => FilePath -> RIO env Bool
isEmptyDirectory dir = do
  isDir <- doesDirectoryExist dir
  unless isDir $ do
    logError "The specified path is not a directory."
    throwString $ "not a directory: " <> dir
  contents <- listDirectory dir
  return $ null contents
