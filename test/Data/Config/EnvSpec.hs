{-# LANGUAGE OverloadedStrings #-}
module Data.Config.EnvSpec
  ( spec
  ) where

import Data.Config (int, optional, text)
import Data.Config.Env (Error(..), fromEnv)
import Data.Validation (AccValidation(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import System.Environment (setEnv)

import qualified Data.Set as Set

spec :: Spec
spec =
  describe "fromEnv" $ do
    describe "success" $ do
      it "text" $ do
        setEnv "ENVSPEC_FOO" "BAR"
        val <- fromEnv "ENVSPEC" $ text "foo"
        val `shouldBe` AccSuccess "BAR"
      it "int" $ do
        setEnv "ENVSPEC_BAZ" "538"
        val <- fromEnv "ENVSPEC" $ int "baz"
        val `shouldBe` AccSuccess 538
      it "optional" $ do
        valBefore <- fromEnv "ENVSPEC" $ optional (int "qux")
        valBefore `shouldBe` AccSuccess Nothing
        setEnv "ENVSPEC_QUX" "538"
        valAfter <- fromEnv "ENVSPEC" $ optional (int "qux")
        valAfter `shouldBe` AccSuccess (Just 538)
    it "missing error" $ do
      val <- fromEnv "ENVSPEC" $ int "moo"
      val `shouldBe` AccFailure (Set.singleton ("moo", VarMissing))
    describe "parse error" $
      it "int" $ do
        setEnv "ENVSPEC_DOO" "RADIO 538"
        val <- fromEnv "ENVSPEC" $ int "doo"
        val `shouldBe` AccFailure (Set.singleton ("doo", ParseError))
