module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude (Unit)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "dummy" do
    Assert.equal true true
