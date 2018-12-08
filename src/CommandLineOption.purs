module CommandLineOption
  ( parse
  ) where

import Bouzuya.CommandLineOption as CommandLineOption
import Bouzuya.CommandLineOption.OptionDefinition (TypedOptionDefinition, booleanOption, maybeStringOption, stringOption)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Prelude (map)

type CommandLineOptions =
  { inFile :: String -- TODO
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
  }

defs ::
  { inFile :: TypedOptionDefinition (Maybe String)
  , inFormat :: TypedOptionDefinition String
  , outFormat :: TypedOptionDefinition String
  , verbose :: TypedOptionDefinition Boolean
  , version :: TypedOptionDefinition Boolean
  }
defs =
  { inFile: maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
  , inFormat: stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
  , outFormat: stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
  , verbose: booleanOption "verbose" (Just 'v') "verbose"
  , version: booleanOption "version" (Just 'V') "show version"
  }

parse :: Array String -> Maybe CommandLineOptions
parse ss = map _.options (hush (CommandLineOption.parse defs ss))
