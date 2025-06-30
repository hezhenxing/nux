{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.Host
  ( hostCmds
  ) where

import           Nux.Host
import           Nux.Options
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
  { delOptNames :: [String]
  } deriving (Show, Eq)

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete host configuration"
  runDel
  (DelOptions <$> some (strArgument ( metavar "NAME"
                       <> help "Host name to be deleted"
                       )))

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  forM_ delOptNames $ \hostname -> do
    logInfo $ "Deleting host configuration for " <> fromString hostname
    let hostDir = flake </> "nix/hosts" </> hostname
    exists <- doesDirectoryExist hostDir
    if exists
      then do
        removeDirectoryRecursive hostDir
        logInfo $ "Host configuration for " <> fromString hostname <> " deleted."
      else logWarn $ "Host configuration for " <> fromString hostname <> " does not exist."
