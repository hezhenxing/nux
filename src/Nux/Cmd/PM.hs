{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Nux.Cmd.PM
  ( pmCmds
  ) where

import RIO
import RIO.Directory
import RIO.File
import RIO.FilePath
import qualified RIO.List as L
import Nux.Expr
import Nux.Options
import Nux.Pretty
import Nux.Util

pmCmds :: Command (RIO App ())
pmCmds = addSubCommands
  "pm"
  "Package manager commands"
  (do addCmd
      delCmd
      listCmd
  )

data AddOptions = AddOptions
  { addOptNames   :: [String]
  , addOptGlobal  :: Bool
  , addOptProgram :: Bool
  , addOptService :: Bool
  } deriving (Show, Eq)

addCmd :: Command (RIO App ())
addCmd = addCommand
  "add"
  "Add package to Nux system"
  runAdd
  (AddOptions <$> some (strArgument ( metavar "NAME"
                                     <> help "Package name to be installed"
                                     ))
              <*> switch ( long "global"
                        <> short 'g'
                        <> help "Add package globally (system-wide) instead of user-specific"
                          )
              <*> switch ( long "program"
                        <> short 'p'
                        <> help "Add package as a program"
                        )
              <*> switch ( long "service"
                        <> short 's'
                        <> help "Add package as a service"
                        )
  )

runAdd :: AddOptions -> RIO App ()
runAdd AddOptions{..} = do
  flake <- view flakeL
  let pkgsDir = if addOptGlobal
                then flake </> "nix/nixosModules/packages"
                else flake </> "nix/homeModules/packages"
  logInfo $ fromString $ "Adding " <> title addOptGlobal
  forM_ addOptNames $ \pname -> do
    let pkgFile = pkgsDir </> pname <.> "nix"
    exist <- doesFileExist pkgFile
    if exist
      then do
        logWarn $ fromString $ "Package already added: " <> pname
      else do
        writeBinaryFile pkgFile $ fromString $ content pname
        nixfmt pkgFile []
        void $ gitC flake "add" [pkgFile]
  void $ gitC flake "commit" ["-m", L.unlines ("Add " <> title addOptGlobal : addOptNames)]
  logInfo $ fromString $ "Successfully added " <> title addOptGlobal <> "!"
  where
    content name =
      if not (addOptProgram || addOptService)
      then auto addOptGlobal name
      else if addOptProgram
        then program name
        else service name
    auto global pname = L.unlines
      [ "{pkgs, lib, options, ...}: let"
      , "inherit (builtins) hasAttr;"
      , "in lib.mkMerge [("
      , "if hasAttr \"" <> pname <> "\" options.services then"
      , service pname
      , "else if hasAttr \"" <> pname <> "\" options.programs then"
      , program pname
      , "else"
      , package global pname
      , ")]"
      ]
    service pname =
      "{ services." <> pname <> ".enable = true; }"
    program pname =
      "{ programs." <> pname <> ".enable = true; }"
    package global pname =
      "{ " <> packages <> " = [pkgs." <> pname <> "]; }"
      where
        packages = if global
          then "environment.systemPackages"
          else "home.packages"

data DelOptions = DelOptions
  { delOptNames   :: [String]
  , delOptGlobal  :: Bool
  } deriving (Show, Eq)

delCmd :: Command (RIO App ())
delCmd = addCommand
  "del"
  "Delete package from Nux system"
  runDel
  (DelOptions <$> some (strArgument ( metavar "NAME"
                                     <> help "Package name to be deleted"
                                     ))
              <*> switch ( long "global"
                        <> short 'g'
                        <> help "Delete package globally (system-wide) instead of user-specific"
                          )
  )

runDel :: DelOptions -> RIO App ()
runDel DelOptions{..} = do
  flake <- view flakeL
  let pkgsDir = if delOptGlobal
                then flake </> "nix/nixosModules/packages"
                else flake </> "nix/homeModules/packages"
  logInfo $ fromString $ "Deleting " <> title delOptGlobal
  forM_ delOptNames $ \pname -> do
    let pkgFile = pkgsDir </> pname <.> "nix"
    exist <- doesFileExist pkgFile
    if not exist
      then logWarn $ fromString $ "Package not found: " <> pname
      else do
        removeFile pkgFile
        void $ gitC flake "rm" [pkgFile]
  void $ gitC flake "commit" ["-m", L.unlines ("Delete " <> title delOptGlobal : delOptNames)]
  logInfo $ fromString $ "Successfully deleted " <> title delOptGlobal <> "!"

data ListOptions = ListOptions
  { listOptGlobal :: Bool
  , listOptAll    :: Bool
  } deriving (Show, Eq)

listCmd :: Command (RIO App ())
listCmd = addCommand
  "list"
  "List packages in Nux system"
  runList
  (ListOptions <$> switch ( long "global"
                          <> short 'g'
                          <> help "List global (system-wide) packages instead of user-specific"
                            )
              <*> switch ( long "all"
                          <> short 'a'
                          <> help "List all packages, both user-specific and global"
                            )
  )

runList :: ListOptions -> RIO App ()
runList ListOptions{..} = do
  flake <- view flakeL
  let upkgs = (False, flake </> "nix/homeModules/packages")
  let gpkgs = (True, flake </> "nix/nixosModules/packages")
  let dirs =
        if listOptAll
          then [gpkgs, upkgs]
          else if listOptGlobal
            then [gpkgs]
            else [upkgs]
  forM_ dirs $ \(global, pkgsDir) -> do
    files <- listDirectory pkgsDir
    let packages = filter (\f -> takeExtension f == ".nix") files
    if (null packages)
      then logInfo "No packages found."
      else do
        logInfo $ fromString $ "List of " <> title global
        forM_ packages $ \pkg -> do
          let pkgName = dropExtension pkg
          logInfo $ fromString pkgName

title :: Bool -> String
title global = if global
  then "global packages"
  else "user packages"
