module Test.Bouzuya.CommandLineOption.ObjectToRecord
  ( tests
  ) where

import Bouzuya.CommandLineOption.ObjectToRecord (toRecord)
import Bouzuya.CommandLineOption.OptionValue as OptionValue
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.ObjectToRecord" do
  test "Boolean/String" do
    let
      obj =
        Object.fromFoldable
          [ Tuple "k1" (OptionValue.fromString "v1")
          , Tuple "k2" (OptionValue.fromBoolean true)
          ]
    Assert.equal
      (Just
        { k1: "v1"
        , k2: true
        })
      (toRecord obj)
  test "Maybe String" do
    let o1 = Object.fromFoldable []
    Assert.equal (Just { k: Nothing :: Maybe String }) (toRecord o1)
    let o2 = Object.fromFoldable [ Tuple "k" (OptionValue.fromString "v") ]
    Assert.equal (Just { k: Just "v" }) (toRecord o2)
