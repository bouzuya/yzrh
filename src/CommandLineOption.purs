module CommandLineOption
  ( parse
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition, booleanOption, stringOption)
import CommandLineOption.OptionObject (OptionObject, toObject)
import CommandLineOption.OptionValue as OptionValue
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Prelude (bind, join, map, pure, (<<<))

type CommandLineOptions =
  { inFile :: String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  }

optionDefinitions :: Array OptionDefinition
optionDefinitions =
  [ stringOption
    { help: "input file"
    , long: "in-file"
    , metavar: "<file>"
    , short: Just 'f'
    , value: Nothing
    }
  , stringOption
    { help: "input file format"
    , long: "in-format"
    , metavar: "<format>"
    , short: Just 'i'
    , value: Just "json"
    }
  , stringOption
    { help: "output file format"
    , long: "out-format"
    , metavar: "<format>"
    , short: Just 'o'
    , value: Just "json"
    }
  , booleanOption
    { help: "verbose"
    , long: "verbose"
    , short: Just 'v'
    }
  ]

parse :: Array String -> Maybe CommandLineOptions
parse = toRecord <<< (toObject optionDefinitions)

toRecord :: OptionObject -> Maybe CommandLineOptions
toRecord o = do
  inFile <- join (map OptionValue.getStringValue (Object.lookup "in-file" o))
  inFormat <- join (map OptionValue.getStringValue (Object.lookup "in-format" o))
  outFormat <- join (map OptionValue.getStringValue (Object.lookup "out-format" o))
  verbose <- join (map OptionValue.getBooleanValue (Object.lookup  "verbose" o))
  pure { inFile, inFormat, outFormat, verbose }
