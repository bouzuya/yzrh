module Test.Main
  ( main
  ) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.PathTemplate as PathTemplate
import Test.RouteConfig as RouteConfig
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  test "dummy" do
    Assert.equal true true
  PathTemplate.tests
  RouteConfig.tests
