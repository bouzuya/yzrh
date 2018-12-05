module CommandLineOption
  ( parse
  ) where

import CommandLineOption.ObjectToRecord as ObjectToRecord
import CommandLineOption.OptionDefinition (OptionDefinition, booleanOption, stringOption)
import CommandLineOption.OptionDefinitionRecordToArray (booleanOption', maybeStringOption', stringOption')
import CommandLineOption.OptionObject as OptionObject
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Prelude (bind, map)

type CommandLineOptions =
  { inFile :: String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
  }

-- TODO
optionDefinitions' =
  { inFile: maybeStringOption' "in-file" (Just 'f') "<file>" "input file" Nothing
  , inFormat: stringOption' "in-format" (Just 'i') "<format>" "input file format" "json"
  , outFormat: stringOption' "out-format" (Just 'o') "<format>" "output file format" "json"
  , verbose: booleanOption' "verbose" (Just 'v') "verbose"
  , version: booleanOption' "version" (Just 'V') "show version"
  }

optionDefinitions :: Array OptionDefinition
optionDefinitions =
  [ stringOption
    { help: "input file"
    , long: "in-file"
    , metavar: "<file>"
    , name: "inFile"
    , short: Just 'f'
    , value: Nothing
    }
  , stringOption
    { help: "input file format"
    , long: "in-format"
    , metavar: "<format>"
    , name: "inFormat"
    , short: Just 'i'
    , value: Just "json"
    }
  , stringOption
    { help: "output file format"
    , long: "out-format"
    , metavar: "<format>"
    , name: "outFormat"
    , short: Just 'o'
    , value: Just "json"
    }
  , booleanOption
    { help: "verbose"
    , long: "verbose"
    , name: "verbose"
    , short: Just 'v'
    }
  , booleanOption
    { help: "show version"
    , long: "version"
    , name: "version"
    , short: Just 'V'
    }
  ]

parse :: Array String -> Maybe CommandLineOptions
parse ss = do
  o <- hush (map _.options (OptionObject.parse optionDefinitions ss))
  ObjectToRecord.toRecord o
