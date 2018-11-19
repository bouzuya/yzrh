module CommandLineOption
  ( parse
  ) where

import Data.Array (foldl)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, const, map, pure, (<<<), (==))

type StringOption =
  { help :: String
  , long :: String
  , metavar :: String
  , short :: Maybe Char
  , value :: Maybe String
  }

type CommandLineOptions =  { inFile :: String, inFormat :: String, outFormat :: String }

type OptionDefinitions = Array StringOption

data OptionName = Long String | Short Char

type OptionObject = Object String

type OptionValue = String

optionDefinitions :: OptionDefinitions
optionDefinitions =
  [ { help: "input file"
    , long: "in-file"
    , metavar: "<file>"
    , short: Just 'f'
    , value: Nothing
    }
  , { help: "input file format"
    , long: "in-format"
    , metavar: "<format>"
    , short: Just 'i'
    , value: Just "json"
    }
  , { help: "output file format"
    , long: "out-format"
    , metavar: "<format>"
    , short: Just 'o'
    , value: Just "json"
    }
  ]

defaults :: OptionDefinitions -> OptionObject
defaults defs =
  foldl
    (\o d ->
      case d.value of
        Nothing -> o
        Just v -> Object.alter (const (Just v)) d.long o)
    Object.empty
    defs

findByOptionName :: OptionName -> OptionDefinitions -> Maybe StringOption
findByOptionName (Long l) = Array.find (\d -> d.long == l)
findByOptionName (Short s) = Array.find (\d -> d.short == Just s)

getOptionName :: String -> Maybe OptionName
getOptionName s =
  let
    p = String.Pattern "-"
  in
    case String.stripPrefix p s of
      Just s' ->
        case String.stripPrefix p s' of
          Just s'' -> Just (Long s'')
          _ -> map Short (CodeUnit.charAt 0 s')
      Nothing -> Nothing

parse :: Array String -> Maybe CommandLineOptions
parse = toRecord <<< (toObject optionDefinitions)

-- ["--name", "value"] -> { name: "value" }
toObject :: OptionDefinitions -> Array String -> OptionObject
toObject defs options = toObject' options (defaults defs)
  where
    toObject' o p =
      case Array.head o of
        Nothing -> p
        Just s ->
          case getOptionName s of
            Nothing -> p -- ERROR: support `--option value` or `-o value` format only
            Just name ->
              -- TODO: long == "" -- double hyphen (--) support
              case findByOptionName name defs of
                Nothing -> p -- ERROR: no such option
                Just d ->
                  case Array.head (Array.drop 1 o) of -- read metavar
                    Nothing -> p -- ERROR: no metavar (end)
                    Just value -> -- TODO: value == "--foo" -- no metavar (next option)
                      toObject' (Array.drop 2 o) (Object.insert d.long value p)

toRecord :: OptionObject -> Maybe CommandLineOptions
toRecord o = do
  inFile <- Object.lookup "in-file" o
  inFormat <- Object.lookup "in-format" o
  outFormat <- Object.lookup "out-format" o
  pure { inFile, inFormat, outFormat }
