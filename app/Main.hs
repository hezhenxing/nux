{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Network.HostName
import           Nux.Cmd
import           Nux.Options
import           Paths_nux         (version)
import           RIO
import           RIO.Process
import           System.Posix.User

main :: IO ()
main = do
  hostname <- getHostName
  username <- getEffectiveUserName
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
      <*> strOption ( long "url"
                     <> metavar "URL"
                     <> value "github:hezhenxing/nuxos"
                     <> help "URL of the NuxOS repository (default: github:hezhenxing/nuxos)"
                    )
      <*> strOption ( long "flake"
                     <> short 'C'
                     <> metavar "FLAKE"
                     <> value "/etc/nuxos"
                     <> help "Path to the NuxOS configuration directory (default: /etc/nuxos)"
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
