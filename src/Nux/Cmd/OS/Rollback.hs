{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.Cmd.OS.Rollback
  ( rollbackCmd
  ) where

import           Nux.Options
import           Nux.Util
import           RIO

data RollbackOptions = RollbackOptions
  { rollbackOptTo :: Int
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
  let toArgs = if rollbackOptTo == 0 then [] else ["--to", show rollbackOptTo]
  void $ exec "nh" $ ["os",  "rollback"] ++ toArgs
  logInfo "Rollback to NuxOS generation successfully."
