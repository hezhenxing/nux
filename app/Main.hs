{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import RIO
import RIO.Directory
import RIO.Process
import Nux.Cmd
import Nux.Options
import Nux.Util (getEnvDefault)
import Paths_nux (version)

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  nuxosFlake <- getEnvDefault "NUXOS_FLAKE" cwd
  (options, cmd) <- simpleOptions
    $(simpleVersion version)
    "Nux"
    "A simple Haskell application"
    (Options
      <$> switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                 )
      <*> strOption ( long "flake"
                     <> short 'f'
                     <> metavar "FLAKE"
                     <> value nuxosFlake
                     <> help "Path to the NuxOS flake"
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
