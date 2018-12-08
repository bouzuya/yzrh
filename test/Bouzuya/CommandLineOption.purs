module Test.Bouzuya.CommandLineOption
  ( tests
  ) where

import Prelude (discard)
import Test.CommandLineOption.ObjectToRecord as ObjectToRecord
import Test.CommandLineOption.OptionObject as OptionObject
import Test.CommandLineOption.RecordToArray as RecordToArray
import Test.Unit (TestSuite, suite)

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption" do
  ObjectToRecord.tests
  OptionObject.tests
  RecordToArray.tests
