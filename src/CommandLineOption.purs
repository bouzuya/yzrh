module CommandLineOption
  ( parse
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition, booleanOption, stringOption)
import CommandLineOption.OptionObject (OptionObject, toObject)
import CommandLineOption.OptionValue as OptionValue
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Prelude (bind, join, map, pure, (<<<))

type CommandLineOptions =
  { inFile :: String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
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
parse = toRecord <<< (toObject optionDefinitions)

toRecord :: Either String { arguments :: Array String, options :: OptionObject } -> Maybe CommandLineOptions
toRecord e = do
  { options: o } <- hush e
  inFile <- join (map OptionValue.getStringValue (Object.lookup "inFile" o))
  inFormat <- join (map OptionValue.getStringValue (Object.lookup "inFormat" o))
  outFormat <- join (map OptionValue.getStringValue (Object.lookup "outFormat" o))
  verbose <- join (map OptionValue.getBooleanValue (Object.lookup  "verbose" o))
  version <- join (map OptionValue.getBooleanValue (Object.lookup  "version" o))
  pure { inFile, inFormat, outFormat, verbose, version }
