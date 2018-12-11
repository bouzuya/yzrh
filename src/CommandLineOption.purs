module CommandLineOption
  ( parse
  ) where

import Bouzuya.CommandLineOption as CommandLineOption
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Prelude (map)

type CommandLineOptions =
  { inFile :: Maybe String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
  }

defs ::
  { inFile :: CommandLineOption.TypedOptionDefinition (Maybe String)
  , inFormat :: CommandLineOption.TypedOptionDefinition String
  , outFormat :: CommandLineOption.TypedOptionDefinition String
  , verbose :: CommandLineOption.TypedOptionDefinition Boolean
  , version :: CommandLineOption.TypedOptionDefinition Boolean
  }
defs =
  { inFile: CommandLineOption.maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
  , inFormat: CommandLineOption.stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
  , outFormat: CommandLineOption.stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
  , verbose: CommandLineOption.booleanOption "verbose" (Just 'v') "verbose"
  , version: CommandLineOption.booleanOption "version" (Just 'V') "show version"
  }

parse :: Array String -> Maybe CommandLineOptions
parse ss = map _.options (hush (CommandLineOption.parse defs ss))
