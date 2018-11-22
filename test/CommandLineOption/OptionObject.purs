module Test.CommandLineOption.OptionObject
  ( tests
  ) where

import CommandLineOption.OptionDefinition (booleanOption, stringOption)
import CommandLineOption.OptionObject (toObject)
import CommandLineOption.OptionValue as OptionValue
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "CommandLineOption.OptionObject" do
  let
    f = toObject
    defs =
      [ stringOption
        { help: "a string option"
        , long: "a-string"
        , metavar: "<a>"
        , short: Just 'a'
        , value: Just "a1"
        }
      , stringOption
        { help: "b string option"
        , long: "b-string"
        , metavar: "<b>"
        , short: Just 'b'
        , value: Nothing
        }
      , booleanOption
        { help: "c boolean option"
        , long: "c-boolean"
        , short: Just 'c'
        }
      , booleanOption
        { help: "d boolean option"
        , long: "d-boolean"
        , short: Just 'd'
        }
      ]
    defaults =
      o
        [ s "a-string" "a1"
        , b "c-boolean" false
        , b "d-boolean" false
        ]
    o es = Object.fromFoldable es
    s k v = Tuple k (OptionValue.fromString v)
    b k v = Tuple k (OptionValue.fromBoolean v)
  suite "long (--foo bar)" do
    test "string option" do
      Assert.equal
        (Right (Object.union (o [s "a-string" "a1"]) defaults))
        (f defs [])
      Assert.equal
        (Right (Object.union (o [s "a-string" "a2"]) defaults))
        (f defs ["--a-string", "a2"])
    test "boolean option" do
      Assert.equal
        (Right (Object.union (o [b "c-boolean" false]) defaults))
        (f defs [])
      Assert.equal
        (Right (Object.union (o [b "c-boolean" true]) defaults))
        (f defs ["--c-boolean"])
    test "string option and boolean option" do
      Assert.equal
        (Right
          (Object.union
            (o
              [ s "a-string" "a2"
              , b "c-boolean" true
              ])
            defaults))
        (f
          defs
          [ "--a-string"
          , "a2"
          , "--c-boolean"
          ])
  suite "long (--foo=bar)" do
    test "string option" do
      Assert.equal
        (Right (Object.union (o [s "a-string" "a2"]) defaults))
        (f defs ["--a-string=a2"])
    -- test "boolean option" -- ERROR
    test "string option and boolean option" do
      Assert.equal
        (Right
          (Object.union
            (o
              [ s "a-string" "a2"
              , b "c-boolean" true
              ])
            defaults))
        (f
          defs
          [ "--a-string=a2"
          , "--c-boolean"
          ])
  suite "short (-f b)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          (Object.union
            (o
              [ s "a-string" "a2"
              , b "c-boolean" true
              ])
            defaults))
        (f
          defs
          [ "-a"
          , "a2"
          , "-c"
          ])
  suite "short (-f=b)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          (Object.union
            (o
              [ s "a-string" "a2"
              , b "c-boolean" true
              ])
            defaults))
        (f
          defs
          [ "-a=a2"
          , "-c"
          ])
  suite "short (-fg)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          (Object.union
            (o
              [ s "a-string" "a2"
              , b "c-boolean" true
              , b "d-boolean" true
              ])
            defaults))
        (f
          defs
          [ "-a=a2"
          , "-cd"
          ])
