module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Test.Options as Options
import Test.PathTemplate as PathTemplate
import Test.RouteConfig as RouteConfig
import Test.Unit.Main as TestUnitMain

main :: Effect Unit
main = TestUnitMain.runTest do
  Options.tests
  PathTemplate.tests
  RouteConfig.tests
