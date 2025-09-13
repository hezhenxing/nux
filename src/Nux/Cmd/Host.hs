{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.Host
  ( hostCmds
  ) where

import           Nux.Host
import           Nux.Options
import           Nux.Process
import           Nux.Util
import           RIO
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List      as L
import qualified RIO.Text      as T

hostCmds :: Command (RIO App ())
hostCmds = addSubCommands
  "host"
  "Host commands"
  (do addCmd
      delCmd
      listCmd
      editCmd
      showCmd
  )

data AddOptions = AddOptions
  { addOptNames    :: [String]
  , addOptCopyFrom :: String
  }

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add host configuration"
  runAdd
  (AddOptions
    <$> some (strArgument ( metavar "NAME"
                                    <> help "Host name to be added"
                          )
              )
    <*> strOption ( long "copy-from"
                           <> short 'c'
                           <> help "Copy from existing host configuration"
                           <> value ""
                  )
  )

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
        if addOptCopyFrom /= ""
          then do
            let srcHostDir = hostsDir </> addOptCopyFrom
            logInfo $ fromString $ "Cloning host configuration from " <> addOptCopyFrom
            cloneHost srcHostDir hostDir
          else do
            logInfo "Generating new host configuration"
            writeBinaryFile hostNix $ fromString $ L.unlines
              [ "with builtins; fromJSON (readFile ./host.json)"
              ]
            writeHost hostFile $ newHost system

cloneHost :: FilePath -> FilePath -> RIO App ()
cloneHost srcHostDir tgtHostDIr  = do
  exists <- doesDirectoryExist srcHostDir
  unless exists $
    throwString $ "Source host directory not found: " <> srcHostDir
  createDirectoryIfMissing True tgtHostDIr
  files <- listDirectory srcHostDir
    <&> filter (`notElem` ["drivers.nix", "hardware.nix"])
  forM_ files $ \f -> do
    let src = srcHostDir </> f
    let dst = tgtHostDIr </> f
    isDir <- doesDirectoryExist src
    if isDir
      then cp src dst
      else copyFile src dst

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

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List existing hosts"
  (const runList)
  (pure ())

runList :: RIO App ()
runList = do
  flake <- view flakeL
  let hostsDir = flake </> "nix/hosts"
  names <- listDirectory hostsDir
  logInfo "Hosts in configuration"
  forM_ names $ \name -> do
    let nixFile = hostsDir </> name </> "default.nix"
    exists <- doesFileExist nixFile
    when exists $
      logInfo $ fromString name

data EditOptions = EditOptions
  { editOptSystem   :: String
  , editOptLanguage :: String
  , editOptTimezone :: String
  }

editCmd :: Command (RIO App ())
editCmd = addCommand
  "edit"
  "Edit host configuration"
  runEdit
  (EditOptions
    <$> strOption ( long "system"
                 <> short 's'
                 <> help "New host system"
                 <> value ""
                  )
    <*> strOption ( long "language"
                 <> short 'l'
                 <> help "New host language"
                 <> value ""
                  )
    <*> strOption ( long "timezone"
                 <> short 't'
                 <> help "New timezone for the host"
                 <> value ""
                  )
  )

runEdit :: EditOptions -> RIO App ()
runEdit EditOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  let hostFile = hostFilePath flake hostname
  exists <- doesFileExist hostFile
  if exists
    then do
      h <- readHost hostFile
      let host = h { hostSystem   = editOptSystem   `nullOr` hostSystem h
                   , hostLanguage = editOptLanguage `nullOr` hostLanguage h
                   , hostTimezone = editOptTimezone `nullOr` hostTimezone h
                   }
      writeHost hostFile host
      logInfo $ fromString $ "Successfully updated host " <> hostname <> "!"
    else
      throwString $ "host not found: " <> hostname

data ShowOptions = ShowOptions
  { showOptConfig :: Bool
  }

showCmd :: Command (RIO App ())
showCmd = addCommand
  "show"
  "Show configuration of host"
  runShow
  (ShowOptions
    <$> switch ( long "config"
              <> help "Show content of configuration"
               )
  )

runShow :: ShowOptions -> RIO App ()
runShow ShowOptions{..} = do
  flake <- view flakeL
  hostname <- view hostL
  logInfo $ fromString $ "Using NuxOS configuration in " <> flake
  let hostFile = hostFilePath flake hostname
  exists <- doesFileExist hostFile
  if exists
    then do
      if showOptConfig
        then do
          content <- readFileUtf8 hostFile
          logInfo $ fromString $ T.unpack content
        else do
          h <- readHost hostFile
          logInfo $ fromString $ "Host configuration of " <> hostname
          logInfo $ fromString $ L.unlines
            [ "  system:      " <> hostSystem h
            , "  language:    " <> hostLanguage h
            , "  timezone:    " <> hostTimezone h
            , "  filesystems: " <> show (hostFileSystems h)
            , "  packages:    " <> show (hostAutos h)
            ]
    else
      throwString $ "host not found: " <> hostname
