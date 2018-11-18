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
    let defaults = { inFormat: "json", outFormat: "json" }
    Assert.equal (defaults { inFormat = "json" }) (parse [])
    Assert.equal (defaults { inFormat = "json" }) (parse ["--in-format", "json"])
    Assert.equal (defaults { inFormat = "routes.rb" }) (parse ["--in-format", "routes.rb"])
    Assert.equal (defaults { outFormat = "json" }) (parse [])
    Assert.equal (defaults { outFormat = "json" }) (parse ["--out-format", "json"])
    Assert.equal (defaults { outFormat = "routes.rb" }) (parse ["--out-format", "routes.rb"])
    Assert.equal
      (defaults { inFormat = "routes.rb", outFormat = "routes.rb" })
      (parse ["--in-format", "routes.rb", "--out-format", "routes.rb"])
