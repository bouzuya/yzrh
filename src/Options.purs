module Options
  ( help
  , parse
  ) where

import Bouzuya.CommandLineOption as CommandLineOption
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.String as String
import Prelude (map)

type CommandLineOptions =
  { help :: Boolean
  , inFile :: Maybe String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
  }

defs ::
  { help :: CommandLineOption.OptionDefinition Boolean
  , inFile :: CommandLineOption.OptionDefinition (Maybe String)
  , inFormat :: CommandLineOption.OptionDefinition String
  , outFormat :: CommandLineOption.OptionDefinition String
  , verbose :: CommandLineOption.OptionDefinition Boolean
  , version :: CommandLineOption.OptionDefinition Boolean
  }
defs =
  { help: CommandLineOption.booleanOption "help" (Just 'h') "show help"
  , inFile: CommandLineOption.maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
  , inFormat: CommandLineOption.stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
  , outFormat: CommandLineOption.stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
  , verbose: CommandLineOption.booleanOption "verbose" (Just 'v') "verbose"
  , version: CommandLineOption.booleanOption "version" (Just 'V') "show version"
  }

help :: String
help =
  String.joinWith
    "\n"
    [ "Usage: yzrh "
    , ""
    , "Options:"
    , "  -h,--help                show help"
    , "  -i,--in-format <format>  input file format"
    , "  -o,--out-format <format> output file format"
    , "  -v,--verbose             verbose"
    , "  -V,--version             show version"
    , ""
    ]

parse :: Array String -> Maybe CommandLineOptions
parse ss = map _.options (hush (CommandLineOption.parse defs ss))
