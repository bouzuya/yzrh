module Test.CommandLineOption.ObjectToRecord
  ( tests
  ) where

import Bouzuya.CommandLineOption.ObjectToRecord (toRecord)
import Bouzuya.CommandLineOption.OptionValue as OptionValue
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude ((==))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.ObjectToRecord" do
  test "" do
    let
      obj =
        Object.fromFoldable
          [ Tuple "k1" (OptionValue.fromString "v1")
          , Tuple "k2" (OptionValue.fromBoolean true)
          ]
    Assert.assert "" (toRecord obj == Just { k1: "v1", k2: true })
