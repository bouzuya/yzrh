module Test.Bouzuya.CommandLineOption.OptionValue
  ( tests
  ) where

import Bouzuya.CommandLineOption.OptionValue (OptionValue, fromBoolean, fromString, getBooleanValue, getStringValue)
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.OptionValue" do
  let
    b :: OptionValue
    b = fromBoolean true
    s :: OptionValue
    s = fromString "s"
  test "getBooleanValue" do
    Assert.equal (Just true) (getBooleanValue b)
    Assert.equal Nothing (getBooleanValue s)
  test "getStringValue" do
    Assert.equal Nothing (getStringValue b)
    Assert.equal (Just "s") (getStringValue s)
