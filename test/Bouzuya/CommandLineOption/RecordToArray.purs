module Test.Bouzuya.CommandLineOption.RecordToArray
  ( tests
  ) where

import Bouzuya.CommandLineOption.Internal.OptionDefinition (NamedOptionDefinition, TypedOptionDefinition, booleanOption, fromTyped, maybeStringOption, stringOption, withName)
import Bouzuya.CommandLineOption.RecordToArray (toArray)
import Data.Maybe (Maybe(..))
import Prelude ((==))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.RecordToArray" do
  let
    f = toArray

    defRecord ::
      { inFile :: TypedOptionDefinition (Maybe String)
      , inFormat :: TypedOptionDefinition String
      , outFormat :: TypedOptionDefinition String
      , verbose :: TypedOptionDefinition Boolean
      , version :: TypedOptionDefinition Boolean
      }
    defRecord =
      { inFile: maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
      , inFormat: stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
      , outFormat: stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
      , verbose: booleanOption "verbose" (Just 'v') "verbose"
      , version: booleanOption "version" (Just 'V') "show version"
      }

    defArray :: Array NamedOptionDefinition
    defArray =
      [ withName "inFile" (fromTyped defRecord.inFile)
      , withName "inFormat" (fromTyped defRecord.inFormat)
      , withName "outFormat" (fromTyped defRecord.outFormat)
      , withName "verbose" (fromTyped defRecord.verbose)
      , withName "version" (fromTyped defRecord.version)
      ]

  test "example" do
    Assert.assert "==" (defArray == (f defRecord))
