{-# LANGUAGE OverloadedStrings #-}
module ExprSpec (spec) where

import Nux.Expr
import Nux.Pretty
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "expr" $ do
    it "null" $
      showPretty NixNull `shouldBe` "null"
    it "path" $
      showPretty (NixPath "/path/to/file") `shouldBe` "/path/to/file"
    it "variable" $
      showPretty (NixVar "foo") `shouldBe` "foo"
    it "boolean true" $
      showPretty (NixBool True) `shouldBe` "true"
    it "boolean false" $
      showPretty (NixBool False) `shouldBe` "false"
    it "integer" $
      showPretty (NixInt 42) `shouldBe` "42"
    it "float" $
      showPretty (NixFloat 3.14) `shouldBe` "3.14"
    it "string" $
      showPretty (NixStr "hello") `shouldBe` "\"hello\""
    it "list" $
      showPretty (NixList [NixInt 1, NixInt 2, NixInt 3])
      `shouldBe` "[\n  1\n  2\n  3\n]"
    it "attrs" $
      showPretty (NixAttrs [("a", NixInt 1), ("b", NixInt 2)])
      `shouldBe` "{\n  a = 1;\n  b = 2;\n}"
    it "function" $
      showPretty (NixFunc (NixArgs [("a", Nothing), ("b", Just (NixInt 2))]) (NixInt 3))
      `shouldBe` "{a, b ? 2}: 3"
    it "function with varargs" $
      showPretty (NixFunc (NixVArgs [("a", Nothing), ("b", Just (NixInt 2))]) (NixInt 3))
      `shouldBe` "{a, b ? 2, ...}: 3"
    it "function with anonymous argument" $
      showPretty (NixFunc NixAnonArg (NixInt 3))
      `shouldBe` "_: 3"
    it "function returns function" $
      showPretty (NixFunc (NixArg "a") (NixFunc (NixArg "b") (NixInt 3)))
      `shouldBe` "a: b: 3"
    it "call" $
      showPretty (NixCall "foo" [NixInt 1, NixStr "bar"])
      `shouldBe` "foo 1 \"bar\""
    it "call with attrs arg" $
      showPretty (NixCall "foo" [NixAttrs [("a", NixInt 1), ("b", NixStr "bar")]])
      `shouldBe` "foo {\n  a = 1;\n  b = \"bar\";\n}"
    it "call with list arg" $
      showPretty (NixCall "foo" [NixList [NixInt 1, NixInt 2], NixStr "bar"])
      `shouldBe` "foo [\n  1\n  2\n] \"bar\""
    it "let in" $
      showPretty (NixLetIn [("x", NixInt 1), ("y", NixInt 2)] (NixCall "foo" [NixVar "x", NixVar "y"]))
      `shouldBe` "let\n  x = 1;\n  y = 2;\nin foo x y"
