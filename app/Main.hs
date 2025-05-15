{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import RIO
import RIO.Directory
import RIO.Process
import Options.Applicative.Simple
import Lib
import Paths_nux (version)

main :: IO ()
main = do
  (options, cmd) <- simpleOptions
    $(simpleVersion version)
    "Nux"
    "A simple Haskell application"
    (Options
      <$> switch ( long "verbose"
                  <> short 'v'
                  <> help "Verbose output?"
                 )
    )
    (addSubCommandsWithOptions
      "say"
      "Say something"
      (SayOptions
        <$> strOption
            ( long "name"
            <> short 'n'
            <> value "Nux"
            <> help "Name to say to"
            <> metavar "NAME"
            )
      )
      (do addCommand
            "hello"
            "Hello, Nux!"
            (const hello)
            (pure ())
          addCommand
            "goodbye"
            "Goodbye, Nux!"
            (const goodbye)
            (pure ())
      )
    )
  lo <- logOptionsHandle stderr (optVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
              { appLogFunc = lf
              , appProcessContext = pc
              , appOptions = options
              }
    in runRIO app $ cmd