module Test.Bouzuya.CommandLineOption
  ( tests
  ) where

import Bouzuya.CommandLineOption (parse)
import Bouzuya.CommandLineOption.OptionDefinition (booleanOption, stringOption)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.CommandLineOption.ObjectToRecord as ObjectToRecord
import Test.CommandLineOption.OptionObject as OptionObject
import Test.CommandLineOption.RecordToArray as RecordToArray
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption" do
  test "usage" do
    let
      defs =
        { s: stringOption "str" (Just 's') "<STRING>" "string option" "default"
        , b: booleanOption "bool" (Just 'b') "boolean option"
        }
      argv = ["--str", "a", "-b", "foo", "bar"]
    Assert.equal
      (Right { arguments: ["foo", "bar"], options: { s: "a", b: true } })
      (parse defs argv)
  ObjectToRecord.tests
  OptionObject.tests
  RecordToArray.tests
