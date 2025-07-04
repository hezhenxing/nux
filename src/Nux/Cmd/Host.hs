{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.Host
  ( hostCmds
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.Util
import           RIO
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List      as L

hostCmds :: Command (RIO App ())
hostCmds = addSubCommands
  "host"
  "Host commands"
  (do addCmd
      delCmd
      --listCmd
  )

data AddOptions = AddOptions
  { addOptNames :: [String]
  }

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add host configuration"
  runAdd
  (AddOptions <$> some (strArgument ( metavar "NAME"
                                    <> help "Host name to be added"
                                    )))

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  isForce <- view forceL
  system <- view systemL
  names <- if null addOptNames
                then (:[]) <$> view hostL
                else pure addOptNames
  let hostsDir = flake </> "nix/hosts"
  forM_ names $ \hostname -> do
    logInfo $ fromString $ "Adding host configuration for " <> hostname
    let hostDir = hostsDir </> hostname
    exist <- doesPathExist hostDir
    if exist && not isForce
      then do
        logWarn $ fromString $ "Host directory already exist: " <> hostDir
      else do
        let hostNix = hostDir </> "default.nix"
        let hostFile = hostDir </> "host.json"
        createDirectoryIfMissing True hostDir
        writeBinaryFile hostNix $ fromString $ L.unlines
          [ "with builtins; fromJSON (readFile ./host.json)"
          ]
        writeHost hostFile $ newHost system

data DelOptions = DelOptions
  { delOptNames           :: [String]
  , delOptRemoveDirectory :: Bool
  } deriving (Show, Eq)

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete host configuration"
  runDel
  (DelOptions
    <$> some (strArgument ( metavar "NAME"
                         <> help "Host name to be deleted"
                          ))
    <*> switch ( long "remove-directory"
              <> short 'R'
              <> help "Remove user directory, normally only user config files are deleted"
               )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  forM_ delOptNames $ \hostname -> do
    logInfo $ "Deleting host configuration for " <> fromString hostname
    let hostDir = flake </> "nix/hosts" </> hostname
    exists <- doesDirectoryExist hostDir
    unless exists $ throwString $ "Host not found: " <> hostname
    if delOptRemoveDirectory
      then do
        removeDirectoryRecursive hostDir
      else do
        removeFile $ hostNixFilePath flake hostname
        removeFile $ hostFilePath flake hostname
        isempty <- isDirectoryEmpty hostDir
        if isempty
          then do
            removeDirectory hostDir
          else do
            logWarn "User directory is not empty after delete config files, user directory not removed"

    logInfo $ fromString $ "Successfully deleted host " <> hostname
