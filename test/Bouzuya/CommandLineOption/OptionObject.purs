module Test.CommandLineOption.OptionObject
  ( tests
  ) where

import Bouzuya.CommandLineOption.OptionDefinition (booleanOption', stringOption')
import Bouzuya.CommandLineOption.OptionObject (parse)
import Bouzuya.CommandLineOption.OptionValue as OptionValue
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.OptionObject" do
  let
    f = parse
    defs =
      [ stringOption'
        { help: "a string option"
        , long: "a-string"
        , metavar: "<a>"
        , name: "aString"
        , short: Just 'a'
        , value: Just "a1"
        }
      , stringOption'
        { help: "b string option"
        , long: "b-string"
        , metavar: "<b>"
        , name: "bString"
        , short: Just 'b'
        , value: Nothing
        }
      , booleanOption'
        { help: "c boolean option"
        , long: "c-boolean"
        , name: "cBoolean"
        , short: Just 'c'
        }
      , booleanOption'
        { help: "d boolean option"
        , long: "d-boolean"
        , name: "dBoolean"
        , short: Just 'd'
        }
      ]
    defaults =
      o
        [ s "aString" "a1"
        , b "cBoolean" false
        , b "dBoolean" false
        ]
    o es = Object.fromFoldable es
    s k v = Tuple k (OptionValue.fromString v)
    b k v = Tuple k (OptionValue.fromBoolean v)
  suite "long (--foo bar)" do
    test "string option" do
      Assert.equal
        (Right
          { arguments: []
          , options: Object.union (o [s "aString" "a1"]) defaults
          })
        (f defs [])
      Assert.equal
        (Right
          { arguments: []
          , options: Object.union (o [s "aString" "a2"]) defaults
          })
        (f defs ["--a-string", "a2"])
    test "boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options: Object.union (o [b "cBoolean" false]) defaults
          })
        (f defs [])
      Assert.equal
        (Right
          { arguments: []
          , options: Object.union (o [b "cBoolean" true]) defaults
          })
        (f defs ["--c-boolean"])
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              Object.union
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "--a-string"
          , "a2"
          , "--c-boolean"
          ])
  suite "long (--foo=bar)" do
    test "string option" do
      Assert.equal
        (Right
          { arguments: []
          , options: Object.union (o [s "aString" "a2"]) defaults
          })
        (f defs ["--a-string=a2"])
    test "boolean option (ERROR)" do
      Assert.equal
        (Left "boolean option can't specify value") -- TODO: improve message
        (f defs ["--c-boolean=true"])
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              Object.union
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "--a-string=a2"
          , "--c-boolean"
          ])
  suite "short (-f b)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              Object.union
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
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
          { arguments: []
          , options:
              Object.union
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "-a=a2"
          , "-c"
          ])
  suite "short (-fg)" do
    test "string option and boolean option" do
      Assert.equal
        (Right
          { arguments: []
          , options:
              Object.union
                (o
                  [ s "aString" "a2"
                  , b "cBoolean" true
                  , b "dBoolean" true
                  ])
                defaults
          })
        (f
          defs
          [ "-a=a2"
          , "-cd"
          ])
    test "-string=value (ERROR)" do
      Assert.equal
        (Left "-abc are boolean options") -- TODO: improve message
        (f defs ["-ab=string"])
    test "-boolean=value (ERROR)" do
      Assert.equal
        (Left "-abc=val is invalid format") -- TODO: improve message
        (f defs ["-cd=true"])
  suite "arguments" do
    test "arguments" do
      Assert.equal
        (Right { arguments: ["arg1", "arg2"], options: defaults })
        (f defs ["arg1", "arg2"])
    test "options arguments" do
      Assert.equal
        (Right
          { arguments: ["arg1", "arg2"]
          , options: Object.union (o [ b "cBoolean" true]) defaults
          })
        (f defs ["-c", "arg1", "arg2"])
    test "arguments short option (ERROR)" do
      Assert.equal
        (Left "invalid option position") -- TODo: improve message
        (f defs ["arg1", "arg2", "-c"])
    test "arguments short options (ERROR)" do
      Assert.equal
        (Left "invalid option position") -- TODo: improve message
        (f defs ["arg1", "arg2", "-cd"])
  suite "other errors" do
    test "--unknown (ERROR)" do
      Assert.equal
        (Left "unknown option") -- TODO: improve message
        (f defs ["--unknown"])
    test "-u (ERROR)" do
      Assert.equal
        (Left "unknown option") -- TODO: improve message
        (f defs ["-u"])
    test "-cu (ERROR)" do
      Assert.equal
        (Left "unknown boolean option") -- TODO: improve message
        (f defs ["-cu"])
    test "no metavar (end) (ERROR)" do
      Assert.equal
        (Left "no metavar (end)") -- TODO: improve message
        (f defs ["--a-string"])
      Assert.equal
        (Left "no metavar (end)") -- TODO: improve message
        (f defs ["-a"])
    test "no metavar (next) (ERROR)" do
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["--a-string", "--c-boolean"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["-a", "--c-boolean"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["--a-string", "-c"])
      Assert.equal
        (Left "no metavar (next)") -- TODO: improve message
        (f defs ["-a", "-c"])