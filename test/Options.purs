module Test.Options
  ( tests
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Options as Options
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Options" do
  let parse = Options.parse
  let defaults = { help: false, inFile: "yas.json", inFormat: "json", outFormat: "json", verbose: false, version: false }
  TestUnit.suite "long (--foo bar)" do
    TestUnit.test "help" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", help = false })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", help = true })
        (parse ["--in-file", "routes.rb", "--help"])
    TestUnit.test "in-format" do
      Assert.equal
        (Just defaults { inFile = Just "yas.json", inFormat = "json" })
        (parse ["--in-file", "yas.json"])
      Assert.equal
        (Just defaults { inFile = Just "yas.json", inFormat = "json" })
        (parse ["--in-file", "yas.json", "--in-format", "json"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", inFormat = "routes.rb" })
        (parse ["--in-file", "routes.rb", "--in-format", "routes.rb"])
    TestUnit.test "out-format" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "json" })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "json" })
        (parse ["--in-file", "routes.rb", "--out-format", "json"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "routes.rb" })
        (parse ["--in-file", "routes.rb", "--out-format", "routes.rb"])
    TestUnit.test "verbose" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", verbose = false })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", verbose = true })
        (parse ["--in-file", "routes.rb", "--verbose"])
    TestUnit.test "version" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", version = false })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", version = true })
        (parse ["--in-file", "routes.rb", "--version"])
    TestUnit.test "all" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", inFormat = "routes.rb", outFormat = "routes.rb", verbose = true })
        (parse ["--in-file", "routes.rb", "--in-format", "routes.rb", "--out-format", "routes.rb", "--verbose"])
  TestUnit.suite "long (--foo=bar)" do
    TestUnit.test "--long=1" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb" })
        (parse ["--in-file=routes.rb"])
  TestUnit.test "short (-f b)" do
    Assert.equal
      (Just defaults { help = true, inFile = Just "routes.rb", inFormat = "routes.rb", outFormat = "routes.rb", verbose = true, version = true })
      (parse ["-f", "routes.rb", "-h", "-i", "routes.rb", "-o", "routes.rb", "-v", "-V"])
  TestUnit.test "short (-f=b)" do
    Assert.equal
      (Just defaults { inFile = Just "routes.rb" })
      (parse ["-f=routes.rb"])
  TestUnit.test "short (-fg)" do
    Assert.equal
      (Just defaults { inFile = Just "routes.rb", verbose = true, version = true })
      (parse ["-f=routes.rb", "-vV"])
