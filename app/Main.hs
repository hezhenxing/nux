{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import RIO
import RIO.Directory
import RIO.Process
import Nux.Cmd
import Nux.Options
import Nux.Util (getEnvDefault, getHostname)
import System.Posix.User (getLoginName)
import Paths_nux (version)

main :: IO ()
main = do
  hostname <- getHostname
  username <- getLoginName
  nuxosFlake <- getEnvDefault "NUXOS_FLAKE" "/etc/nuxos"
  (options, cmd) <- simpleOptions
    $(simpleVersion version)
    "Nux"
    "A simple Haskell application"
    (Options
      <$> switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                 )
      <*> switch ( long "force"
                     <> help "Force overwrite existing files"
                    )
      <*> strOption ( long "flake"
                     <> short 'C'
                     <> metavar "FLAKE"
                     <> value nuxosFlake
                     <> help "Path to the NuxOS flake directory (default: /etc/nuxos)"
                    )
      <*> strOption ( long "system"
                     <> short 's'
                     <> metavar "SYSTEM"
                     <> value "x86_64-linux"
                     <> help "NuxOS system name (default: x86_64-linux)"
                    )
      <*> strOption ( long "host"
                     <> short 'H'
                     <> metavar "HOST"
                     <> value hostname
                     <> help ("NuxOS host name (default: " <> hostname <> ")")
                    )
      <*> strOption ( long "user"
                     <> short 'U'
                     <> metavar "USER"
                     <> value username
                     <> help ("NuxOS user name (default: " <> username <> ")")
                    )
    )
    nux
  lo <- logOptionsHandle stderr (optVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
              { appLogFunc = lf
              , appProcessContext = pc
              , appOptions = options
              }
    in runRIO app $ cmd
