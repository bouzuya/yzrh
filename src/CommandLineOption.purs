module CommandLineOption
  ( parse
  ) where

import CommandLineOption.ObjectToRecord as ObjectToRecord
import CommandLineOption.OptionDefinition (TypedOptionDefinition, booleanOption, maybeStringOption, stringOption)
import CommandLineOption.RecordToArray as RecordToArray
import CommandLineOption.OptionObject as OptionObject
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Prelude (bind, map, pure)

type CommandLineOptions =
  { inFile :: String
  , inFormat :: String
  , outFormat :: String
  , verbose :: Boolean
  , version :: Boolean
  }

optionDefinitions ::
  { inFile :: TypedOptionDefinition (Maybe String)
  , inFormat :: TypedOptionDefinition String
  , outFormat :: TypedOptionDefinition String
  , verbose :: TypedOptionDefinition Boolean
  , version :: TypedOptionDefinition Boolean
  }
optionDefinitions =
  { inFile: maybeStringOption "in-file" (Just 'f') "<file>" "input file" Nothing
  , inFormat: stringOption "in-format" (Just 'i') "<format>" "input file format" "json"
  , outFormat: stringOption "out-format" (Just 'o') "<format>" "output file format" "json"
  , verbose: booleanOption "verbose" (Just 'v') "verbose"
  , version: booleanOption "version" (Just 'V') "show version"
  }

parse :: Array String -> Maybe CommandLineOptions
parse ss = do
  defs <- pure (RecordToArray.toArray optionDefinitions)
  o <- hush (map _.options (OptionObject.parse defs ss))
  ObjectToRecord.toRecord o
