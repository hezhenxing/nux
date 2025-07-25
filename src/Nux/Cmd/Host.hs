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
  , editOptProfile  :: String
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
    <*> strOption ( long "profile"
                 <> short 'r'
                 <> help "New profile for the host"
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
                   , hostProfile  = editOptProfile  `nullOr` hostProfile h
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
            , "  profile:     " <> hostProfile h
            , "  filesystems: " <> show (hostFileSystems h)
            , "  packages:    " <> show (hostAutos h)
            ]
    else
      throwString $ "host not found: " <> hostname
