{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Util where

import           RIO
import           RIO.Char                 (isSpace)
import           RIO.Directory
import qualified RIO.List                 as L
import qualified RIO.List.Partial         as L
import qualified System.Environment.Blank as Env

anyM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
anyM f = foldr (orM . f) (pure False)

allM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m Bool
allM f = foldr (andM . f) (pure True)

orM :: Monad m => m Bool -> m Bool -> m Bool
orM m1 m2 = m1 >>= \x -> if x then return True else m2

andM :: Monad m => m Bool -> m Bool -> m Bool
andM m1 m2 = m1 >>= \x -> if x then m2 else return False

split :: Eq a => a -> [a] -> [[a]]
split a as = case rest of
  []   -> [chunk]
  _:rs -> chunk : split a rs
  where (chunk, rest) = break (==a) as

nullOr :: Foldable t => t a -> t a -> t a
nullOr a b = if null a then b else a

emptyOrJust :: (Monoid a, Eq a) => a -> Maybe a
emptyOrJust a = if a == mempty then Nothing else Just a

zeroOrJust :: (Num a, Eq a) => a -> Maybe a
zeroOrJust a = if a == 0 then Nothing else Just a

zeroOrJust2 :: (Eq a, Num a) => a -> Maybe a -> Maybe a
zeroOrJust2 a b = if a == 0 then b else Just a

zeroOr :: (Eq a, Num a) => a -> a -> a
zeroOr a b = if a == 0 then b else a

splitOptions :: String -> [String]
splitOptions s
  = s
  & split ','
  & map trim
  & filter (/= "")

trimL :: String -> String
trimL = L.dropWhile isSpace

trimR :: String -> String
trimR = L.dropWhileEnd isSpace

trim :: String -> String
trim = trimL . trimR

isDirectoryEmpty :: FilePath -> RIO env Bool
isDirectoryEmpty dir = do
  isDir <- doesDirectoryExist dir
  unless isDir $ do
    throwString $ "not a directory: " <> dir
  contents <- listDirectory dir
  return $ null contents

removeDirectoryIfEmpty :: FilePath -> RIO env ()
removeDirectoryIfEmpty dir =
  whenM (isDirectoryEmpty dir) (removeDirectory dir)

getEnvDefault :: MonadIO m => String -> String -> m String
getEnvDefault = (liftIO .) . Env.getEnvDefault

currentTimeZone :: MonadUnliftIO m => m String
currentTimeZone = do
  tz <- getEnvDefault "TZ" ""
  if tz /= ""
    then
      return tz
    else do
      getSymbolicLinkTarget "/etc/localtime" & tryAny >>= \case
        Left _ -> return ""
        Right fp -> return $ L.dropPrefix "/etc/zoneinfo/" fp

followLink :: FilePath -> RIO env FilePath
followLink path = do
  isLink <- pathIsSymbolicLink path
  if isLink
    then getSymbolicLinkTarget path
    else return path

printTable :: [[String]] -> String
printTable rows = L.unlines $ map putRow rows
  where
    widths = map ((+1) . L.maximum . map length ) (L.transpose rows)
    putCell n s = s <> L.replicate (n - length s) ' '
    putRow = concat . zipWith putCell widths
