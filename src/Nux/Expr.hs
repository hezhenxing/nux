{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Nux.Expr where

import RIO
import Nux.Pretty

simpleArgs :: [String] -> NixArg
simpleArgs = NixArgs . map (\k -> (k, Nothing))

simpleVArgs :: [String] -> NixArg
simpleVArgs = NixVArgs . map (\k -> (k, Nothing))

data NixArg
  = NixAnonArg
  | NixArg String
  | NixArgs [(String, Maybe NixExpr)]
  | NixVArgs [(String, Maybe NixExpr)]
  deriving (Eq)

instance Show NixArg where
  show NixAnonArg = "_"
  show (NixArg s) = s
  show (NixArgs m) = showArgs m
  show (NixVArgs m) = showVArgs m

instance Pretty NixArg where
  pretty NixAnonArg = "_"
  pretty (NixArg s) = pretty s
  pretty (NixArgs m) = prettyArgs m
  pretty (NixVArgs m) = prettyVArgs m

data NixExpr
  = NixNull
  | NixPath FilePath
  | NixInt Int
  | NixFloat Double
  | NixStr String
  | NixBool Bool
  | NixList [NixExpr]
  | NixAttrs [(String, NixExpr)]
  | NixFunc NixArg NixExpr
  | NixCall String [NixExpr]
  | NixVar String
  | NixParen NixExpr
  | NixLetIn [(String, NixExpr)] NixExpr
  | NixIndented [String]
  | NixInherit (Maybe String) [String]
  | NixWith String NixExpr
  deriving (Eq)

instance Show NixExpr where
  show NixNull = "null"
  show (NixPath path) = path
  show (NixInt i) = show i
  show (NixFloat f) = show f
  show (NixStr s) = "\"" ++ s ++ "\""
  show (NixBool b) = if b then "true" else "false"
  show (NixList xs) = showList xs
  show (NixAttrs m) = showAttrs m
  show (NixFunc arg body) = show arg ++ ": " ++ show body
  show (NixCall name args) = name ++ " " ++ showList' args
  show (NixVar name) = name
  show (NixParen expr) = "(" ++ show expr ++ ")"
  show (NixLetIn bindings body) =
    "let " ++ (showAttrs' bindings) ++ "in " ++ show body
  show (NixIndented ls) =
    "''\n" ++ unlines ls ++ "''"
  show (NixInherit Nothing names) =
    "inherit " ++ unwords names ++ ";"
  show (NixInherit (Just name) names) =
    "inherit (" ++ name ++ ") " ++ unwords names ++ ";"
  show (NixWith name expr) =
    "with " ++ name ++ "; " ++ show expr

instance Pretty NixExpr where
  pretty NixNull = "null"
  pretty (NixPath path) = pretty path
  pretty (NixInt i) = pretty i
  pretty (NixFloat f) = pretty f
  pretty (NixStr s) = dquotes (pretty s)
  pretty (NixBool b) = if b then "true" else "false"
  pretty (NixList xs) = encloseWith "[" "]" $ map pretty xs
  pretty (NixAttrs m) = encloseWith "{" "}" $ map prettyAttr m
  pretty (NixFunc arg body) = pretty arg <> ":" <+> pretty body
  pretty (NixCall name args) = pretty name <+> hsep (map pretty args)
  pretty (NixVar name) = pretty name
  pretty (NixParen expr) = parens (pretty expr)
  pretty (NixLetIn bindings body) =
    (encloseWith "let" "in" (map prettyBinding bindings)) <+> pretty body
    where
      prettyBinding (name, expr) = pretty name <+> "=" <+> pretty expr <> ";"
  pretty (NixIndented ls) = encloseWith "''" "''" $ map pretty ls
  pretty (NixInherit Nothing names) =
    "inherit" <+> hsep (map pretty names) <> ";"
  pretty (NixInherit (Just name) names) =
    "inherit" <+> parens (pretty name) <+> hsep (map pretty names) <> ";"
  pretty (NixWith name expr) =
    "with" <+> pretty name <> ";" <+> pretty expr

class ToNixExpr a where
  toNixExpr :: a -> NixExpr

instance ToNixExpr NixExpr where
  toNixExpr = id

instance ToNixExpr String where
  toNixExpr = NixStr

instance ToNixExpr Int where
  toNixExpr = NixInt

instance ToNixExpr Double where
  toNixExpr = NixFloat

instance ToNixExpr Bool where
  toNixExpr = NixBool

instance {-# OVERLAPPABLE #-} ToNixExpr a => ToNixExpr [a] where
  toNixExpr = NixList . fmap toNixExpr

instance ToNixExpr a => ToNixExpr (Maybe a) where
  toNixExpr Nothing  = NixNull
  toNixExpr (Just x) = toNixExpr x

instance ToNixExpr a => ToNixExpr [(String, a)] where
  toNixExpr = NixAttrs . fmap (\(k, v) -> (k, toNixExpr v))

writeShellScriptBin :: String -> [String] -> NixExpr
writeShellScriptBin name ls =
  NixCall "pkgs.writeShellScriptBin" [NixStr name, (NixIndented ls)]


nuxFlake :: NixExpr
nuxFlake = NixAttrs
  [ ( "inputs.flakelight-haskell.url"
    , NixStr "github:hezhenxing/flakelight-haskell"
    )
  , ( "outputs"
    , NixFunc
      (NixVArgs [("flakelight-haskell", Nothing)])
      (NixCall "flakelight-haskell"
        [ NixPath "./."
        , NixAttrs
          [ ( "devShell.packages"
            , NixFunc
              (NixArg "pkgs")
              (NixLetIn
                [ ("repl"   , writeShellScriptBin "repl"    [ "hpack", "cabal repl"])
                , ("runtest", writeShellScriptBin "runtest" [ "hpack", "cabal test"])
                ]
                ( NixWith "pkgs"
                  (NixList
                    [ NixVar "hpack"
                    , NixVar "cabal-install"
                    , NixVar "nixfmt"
                    , NixVar "repl"
                    , NixVar "runtest"
                    ]
                  )
                )
              )
            )
          ]
        ]
      )
    )
  ]
