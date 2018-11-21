module CommandLineOption
  ( parse
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition(..))
import CommandLineOption.OptionObject (OptionObject, getBooleanValue, getStringValue, toObject)
import Data.Maybe (Maybe(..))
import Prelude (bind, pure, (<<<))

type CommandLineOptions =
  { inFile :: String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  }

optionDefinitions :: Array OptionDefinition
optionDefinitions =
  [ StringOption
    { help: "input file"
    , long: "in-file"
    , metavar: "<file>"
    , short: Just 'f'
    , value: Nothing
    }
  , StringOption
    { help: "input file format"
    , long: "in-format"
    , metavar: "<format>"
    , short: Just 'i'
    , value: Just "json"
    }
  , StringOption
    { help: "output file format"
    , long: "out-format"
    , metavar: "<format>"
    , short: Just 'o'
    , value: Just "json"
    }
  , BooleanOption
    { help: "verbose"
    , long: "verbose"
    , short: Just 'v'
    }
  ]

parse :: Array String -> Maybe CommandLineOptions
parse = toRecord <<< (toObject optionDefinitions)

toRecord :: OptionObject -> Maybe CommandLineOptions
toRecord o = do
  inFile <- getStringValue "in-file" o
  inFormat <- getStringValue "in-format" o
  outFormat <- getStringValue "out-format" o
  verbose <- getBooleanValue "verbose" o
  pure { inFile, inFormat, outFormat, verbose }
