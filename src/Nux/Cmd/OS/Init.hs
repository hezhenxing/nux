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
import Nux.Options
import qualified RIO.List as L

data InitOptions = InitOptions
  { initOptUrl      :: String
  } deriving (Show, Eq)

initCmd :: Command (RIO App ())
initCmd = addCommand
  "init"
  "Initialize Nux in the current directory"
  run
  opts

opts :: Parser InitOptions
opts = InitOptions
  <$> strOption ( long "url"
                <> help "URL of the NuxOS repository"
                <> value "github:hezhenxing/nuxos"
                )

run :: InitOptions -> RIO App ()
run InitOptions{..} = do
  flake <- view flakeL
  isForce <- view forceL
  logInfo $ fromString $ "Starting Nux initialization in " <> flake
  createDirectoryIfMissing True flake
  isEmpty <- isEmptyDirectory flake
  unless isEmpty $ do
    if isForce
      then do
        logWarn "Directory is not empty, Forcing overwrite existing files..."
      else do
        logError "The target directory is not empty."
        throwString $ "directory not empty: " <> flake
  logInfo $ fromString $ "Initializing Nux in directory " <> flake
  let flakeFile = flake </> "flake.nix"
  writeBinaryFile flakeFile $ fromString $ L.unlines
    [ "{"
    , "  inputs.nuxos.url = \"" <> initOptUrl <> "\";"
    , "  outputs = inputs: inputs.nuxos ./. {"
    , "    inherit inputs;"
    , "  };"
    , "}"
    ]
  logInfo "Nux initialized successfully."

isEmptyDirectory :: HasLogFunc env => FilePath -> RIO env Bool
isEmptyDirectory dir = do
  isDir <- doesDirectoryExist dir
  unless isDir $ do
    logError "The specified path is not a directory."
    throwString $ "not a directory: " <> dir
  contents <- listDirectory dir
  return $ null contents
