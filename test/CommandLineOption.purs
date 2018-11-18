module Test.CommandLineOption
  ( tests
  ) where

import CommandLineOption (parse)
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "CommandLineOption" do
  test "parse" do
    let defaults = { inFile: "yas.json", inFormat: "json", outFormat: "json" }
    Assert.equal
      (Just defaults { inFile = "yas.json", inFormat = "json" })
      (parse ["--in-file", "yas.json"])
    Assert.equal
      (Just defaults { inFile = "yas.json", inFormat = "json" })
      (parse ["--in-file", "yas.json", "--in-format", "json"])
    Assert.equal
      (Just defaults { inFile = "routes.rb", inFormat = "routes.rb" })
      (parse ["--in-file", "routes.rb", "--in-format", "routes.rb"])
    Assert.equal
      (Just defaults { inFile = "routes.rb", outFormat = "json" })
      (parse ["--in-file", "routes.rb"])
    Assert.equal
      (Just defaults { inFile = "routes.rb", outFormat = "json" })
      (parse ["--in-file", "routes.rb", "--out-format", "json"])
    Assert.equal
      (Just defaults { inFile = "routes.rb", outFormat = "routes.rb" })
      (parse ["--in-file", "routes.rb", "--out-format", "routes.rb"])
    Assert.equal
      (Just defaults { inFile = "routes.rb", inFormat = "routes.rb", outFormat = "routes.rb" })
      (parse ["--in-file", "routes.rb", "--in-format", "routes.rb", "--out-format", "routes.rb"])
