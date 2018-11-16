module Test.CommandLineOption
  ( tests
  ) where

import CommandLineOption (parse)
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "CommandLineOption" do
  test "parse" do
    Assert.equal { to: "json" } (parse [])
    Assert.equal { to: "json" } (parse ["--to", "json"])
    Assert.equal { to: "routes.rb" } (parse ["--to", "routes.rb"])
