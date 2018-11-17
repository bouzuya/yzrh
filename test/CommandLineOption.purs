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
    let defaults = { from: "json", to: "json" }
    Assert.equal (defaults { from = "json" }) (parse [])
    Assert.equal (defaults { from = "json" }) (parse ["--from", "json"])
    Assert.equal (defaults { from = "routes.rb" }) (parse ["--from", "routes.rb"])
    Assert.equal (defaults { to = "json" }) (parse [])
    Assert.equal (defaults { to = "json" }) (parse ["--to", "json"])
    Assert.equal (defaults { to = "routes.rb" }) (parse ["--to", "routes.rb"])
    Assert.equal (defaults { from = "routes.rb", to = "routes.rb" }) (parse ["--from", "routes.rb", "--to", "routes.rb"])
