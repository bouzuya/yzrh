module CommandLineOption.OptionObject
  ( OptionObject
  , toObject
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition, getDefaultValue, getLongName, getShortName, isValueRequired)
import CommandLineOption.OptionValue (OptionValue)
import CommandLineOption.OptionValue as OptionValue
import Data.Array (foldl)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (const, map, (==))

data OptionName = Long String | Short Char

type OptionObject = Object OptionValue

defaultValues :: Array OptionDefinition -> OptionObject
defaultValues defs =
  foldl
    (\o d -> Object.alter (const (getDefaultValue d)) (getLongName d) o)
    Object.empty
    defs

findByOptionName :: OptionName -> Array OptionDefinition -> Maybe OptionDefinition
findByOptionName name =
  case name of
    Long l -> Array.find (\d -> getLongName d == l)
    Short s -> Array.find (\d -> getShortName d == Just s)

getOptionName :: String -> Maybe OptionName
getOptionName s =
  let
    p = String.Pattern "-"
  in
    case String.stripPrefix p s of
      Just s' ->
        case String.stripPrefix p s' of
          Just s'' -> Just (Long s'') -- TODO: `--foo=bar`
          _ -> map Short (CodeUnit.charAt 0 s')
      Nothing -> Nothing

-- ["--name", "value"] -> { name: "value" }
toObject :: Array OptionDefinition -> Array String -> OptionObject
toObject defs options = toObject' options (defaultValues defs)
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
                  let
                    long = getLongName d
                  in
                    if isValueRequired d then
                      case Array.head (Array.drop 1 o) of -- read metavar
                        Nothing -> p -- ERROR: no metavar (end)
                        Just value -> -- TODO: value == "--foo" -- no metavar (next option)
                          toObject'
                            (Array.drop 2 o)
                            (Object.insert long (OptionValue.fromString value) p)
                    else
                      toObject'
                        (Array.drop 1 o)
                        (Object.insert long (OptionValue.fromBoolean true) p)
