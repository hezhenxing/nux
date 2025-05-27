{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.Cmd.OS
  ( osCmds
  ) where

import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import Nux.Options
import Nux.Cmd.OS.Init
import Nux.Cmd.OS.Switch
import Nux.Cmd.OS.Install
import Nux.Cmd.OS.VM
import Nux.Cmd.OS.Pull

osCmds :: Command (RIO App ())
osCmds = addSubCommands
  "os"
  "OS commands"
  (do initCmd
      pullCmd
      installCmd
      addCmd
      removeCmd
      switchCmd
      vmCmd
  )

data AddOptions = AddOptions
  { addOptNames :: [String]
  }

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add package to Nux system"
  runAdd
  (AddOptions <$> some (strArgument ( metavar "NAME"
                                    <> help "Package name to be installed"
                                    )))

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  forM_ addOptNames $ \pname -> do
    let nixFile = homeModuleFile pname
    exist <- doesFileExist nixFile
    if exist
      then logWarn $ fromString $ "Package already added: " <> pname
      else writeBinaryFile nixFile $ fromString $ "{ programs." <> pname <> ".enable = true;}"

data RemoveOptions = RemoveOptions
  { removeOptNames :: [String]
  }

removeCmd :: Command (RIO App ())
removeCmd = addCommand
  "remove"
  "Remove package to Nux system"
  runRemove
  (RemoveOptions <$> some (strArgument ( metavar "NAME"
                                    <> help "Package name to be installed"
                                    )))

runRemove :: RemoveOptions -> RIO App ()
runRemove RemoveOptions{..} = do
  forM_ removeOptNames $ \pname -> do
    let nixFile = homeModuleFile pname
    exist <- doesFileExist nixFile
    if exist
      then removeFile nixFile
      else logWarn $ fromString $ "Package not found: " <> pname

homeModuleFile :: String -> FilePath
homeModuleFile name = "nix/homeModules" </> name <.> "nix"
