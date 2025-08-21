{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Rollback
  ( rollbackCmd
  ) where

import           Nux.Options
import           Nux.Process
import           RIO
import           SimplePrompt

data RollbackOptions = RollbackOptions
  { rollbackOptTo  :: Int
  }

rollbackCmd :: Command (RIO App ())
rollbackCmd = addCommand
  "rollback"
  "Rollback to previous or given NuxOS generation"
  runRollback
  (RollbackOptions
    <$> option auto ( long "to"
                   <> short 't'
                   <> help "The number of generation"
                   <> value 0
                    )
  )

runRollback :: RollbackOptions -> RIO App ()
runRollback RollbackOptions{..} = do
  yes <- view yesL
  toArgs <- if rollbackOptTo == 0
    then do
      logInfo "Will rollback to previous generation"
      return []
    else do
      logInfo $ fromString $ "Will rollback to generation " <> show rollbackOptTo
      return ["--to", show rollbackOptTo]
  unless yes $ do
    y <- yesNo "Do you want to continue the rollback"
    unless y $ die "user cancelled rollback"
  run $ cmd "nh" & arg "os" & arg "rollback" & args toArgs
  logInfo "Rollback to NuxOS generation successfully."
