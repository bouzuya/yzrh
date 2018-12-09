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
  let defaults = { inFile: "yas.json", inFormat: "json", outFormat: "json", verbose: false, version: false }
  suite "long (--foo bar)" do
    test "in-format" do
      Assert.equal
        (Just defaults { inFile = Just "yas.json", inFormat = "json" })
        (parse ["--in-file", "yas.json"])
      Assert.equal
        (Just defaults { inFile = Just "yas.json", inFormat = "json" })
        (parse ["--in-file", "yas.json", "--in-format", "json"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", inFormat = "routes.rb" })
        (parse ["--in-file", "routes.rb", "--in-format", "routes.rb"])
    test "out-format" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "json" })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "json" })
        (parse ["--in-file", "routes.rb", "--out-format", "json"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", outFormat = "routes.rb" })
        (parse ["--in-file", "routes.rb", "--out-format", "routes.rb"])
    test "verbose" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", verbose = false })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", verbose = true })
        (parse ["--in-file", "routes.rb", "--verbose"])
    test "version" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", version = false })
        (parse ["--in-file", "routes.rb"])
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", version = true })
        (parse ["--in-file", "routes.rb", "--version"])
    test "all" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb", inFormat = "routes.rb", outFormat = "routes.rb", verbose = true })
        (parse ["--in-file", "routes.rb", "--in-format", "routes.rb", "--out-format", "routes.rb", "--verbose"])
  suite "long (--foo=bar)" do
    test "--long=1" do
      Assert.equal
        (Just defaults { inFile = Just "routes.rb" })
        (parse ["--in-file=routes.rb"])
  test "short (-f b)" do
    Assert.equal
      (Just defaults { inFile = Just "routes.rb", inFormat = "routes.rb", outFormat = "routes.rb", verbose = true, version = true })
      (parse ["-f", "routes.rb", "-i", "routes.rb", "-o", "routes.rb", "-v", "-V"])
  test "short (-f=b)" do
    Assert.equal
      (Just defaults { inFile = Just "routes.rb" })
      (parse ["-f=routes.rb"])
  test "short (-fg)" do
    Assert.equal
      (Just defaults { inFile = Just "routes.rb", verbose = true, version = true })
      (parse ["-f=routes.rb", "-vV"])
