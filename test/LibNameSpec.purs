module Test.LibSpec where

import Prelude

import Lib as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "Lib" do
    it "should be one" do
      ME.x `shouldEqual` 0