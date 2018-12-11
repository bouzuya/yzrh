module Test.Bouzuya.CommandLineOption
  ( tests
  ) where

import Bouzuya.CommandLineOption (booleanOption, maybeStringOption, stringOption, parse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.Bouzuya.CommandLineOption.Internal.OptionDefinition as OptionDefinition
import Test.Bouzuya.CommandLineOption.ObjectToRecord as ObjectToRecord
import Test.Bouzuya.CommandLineOption.OptionObject as OptionObject
import Test.Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption" do
  test "usage" do
    let
      defs =
        { s: stringOption "str" (Just 's') "<STRING>" "string option" "default"
        , b: booleanOption "bool" (Just 'b') "boolean option"
        , m: maybeStringOption "maybe" (Just 'm') "<MAYBE>" "maybe option" (Just "maybe default")
        }
    Assert.equal
      (Right
        { arguments: ["foo", "bar"]
        , options: { s: "a", b: true, m: Just "c" }
        })
      (parse defs ["--str", "a", "-b", "-m", "c", "foo", "bar"])
    Assert.equal
      (Right
        { arguments: []
        , options: { s: "a", b: false, m: Just "maybe default" }
        })
      (parse defs ["--str", "a"])
  ObjectToRecord.tests
  OptionDefinition.tests
  OptionObject.tests
  RecordToArray.tests
