{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Nux.Pretty
  ( module Prettyprinter
  , encloseWith
  , prettyAttr
  , prettyAttrs
  , prettyArg
  , prettyArgs
  , prettyVArgs
  , showPretty
  , showArg
  , showArgs
  , showVArgs
  , showAttr
  , showAttrs
  , showAttrs'
  , showList
  , showList'
  ) where

import           Prettyprinter
import           RIO
import qualified RIO.List      as L

encloseWith :: Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseWith l r [] = l <+> r
encloseWith l r xs =
     l
  <> line
  <> indent 2 (vcat xs)
  <> line
  <> r

prettyAttr :: Pretty v => (String, v) -> Doc ann
prettyAttr (k, v) = pretty k <+> "=" <+> pretty v <> ";"

prettyAttrs :: Pretty v => [(String, v)] -> Doc ann
prettyAttrs m = encloseWith "{" "}" $ map prettyAttr m

prettyArg :: Pretty v => (String, Maybe v) -> Doc ann
prettyArg (k, Nothing) = pretty k
prettyArg (k, Just v)  = pretty k <> " ? " <> pretty v

prettyArgs :: Pretty v => [(String, Maybe v)] -> Doc ann
prettyArgs m = braces $ hsep $ punctuate "," $ map prettyArg $ m

prettyVArgs :: Pretty v => [(String, Maybe v)] -> Doc ann
prettyVArgs m = prettyArgs $ m ++ [("...", Nothing)]

showPretty :: Pretty a => a -> String
showPretty = show . pretty

showArg :: Show v => (String, Maybe v) -> String
showArg (k, Nothing) = k
showArg (k, Just v)  = k ++ " ? " ++ show v

showArgs :: Show v => [(String, Maybe v)] -> String
showArgs m = "{" ++ L.intercalate ", " (map showArg m) ++ "}"

showVArgs :: Show v => [(String, Maybe v)] -> String
showVArgs m = "{" ++ L.intercalate ", " (map showArg m) ++ ", ...}"

showAttr :: Show v => (String, v) -> String
showAttr (k, v) = k ++ "=" ++ show v ++ ";"

showAttrs :: Show v => [(String, v)] -> String
showAttrs = wrapWith "{" "}" . showAttrs'

showAttrs' :: Show v => [(String, v)] -> String
showAttrs' = concatMap showAttr

showList :: Show v => [v] -> String
showList = wrapWith "[" "]" . showList'

showList' :: Show v => [v] -> String
showList' = L.intercalate " " . map show

wrapWith :: String -> String -> String -> String
wrapWith l r s = l ++ s ++ r
