module Test.CommandLineOption.OptionDefinitionRecordToArray
  ( tests
  ) where

import Prelude ((==))

import Bouzuya.CommandLineOption.OptionDefinition (NamedOptionDefinition, TypedOptionDefinition, booleanOption, booleanOption', maybeStringOption, stringOption, stringOption')
import Bouzuya.CommandLineOption.RecordToArray (toArray)
import Data.Maybe (Maybe(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.OptionDefinitionRecordToArray" do
  let
    f = toArray

    optionDefinitions' ::
      { inFile :: TypedOptionDefinition (Maybe String)
      , inFormat :: TypedOptionDefinition String
      , outFormat :: TypedOptionDefinition String
      , verbose :: TypedOptionDefinition Boolean
      , version :: TypedOptionDefinition Boolean
      }
    optionDefinitions' =
      { inFile: maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
      , inFormat: stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
      , outFormat: stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
      , verbose: booleanOption "verbose" (Just 'v') "verbose"
      , version: booleanOption "version" (Just 'V') "show version"
      }

    optionDefinitions :: Array NamedOptionDefinition
    optionDefinitions =
      [ stringOption'
        { help: "input file"
        , long: "in-file"
        , metavar: "<file>"
        , name: "inFile"
        , short: Just 'f'
        , value: Nothing
        }
      , stringOption'
        { help: "input file format"
        , long: "in-format"
        , metavar: "<format>"
        , name: "inFormat"
        , short: Just 'i'
        , value: Just "json"
        }
      , stringOption'
        { help: "output file format"
        , long: "out-format"
        , metavar: "<format>"
        , name: "outFormat"
        , short: Just 'o'
        , value: Just "json"
        }
      , booleanOption'
        { help: "verbose"
        , long: "verbose"
        , name: "verbose"
        , short: Just 'v'
        }
      , booleanOption'
        { help: "show version"
        , long: "version"
        , name: "version"
        , short: Just 'V'
        }
      ]

  test "example" do
    Assert.assert "==" (optionDefinitions == (f optionDefinitions'))
