module CommandLineOption.OptionObject
  ( OptionObject
  , OptionValue
  , getBooleanValue
  , getStringValue
  , toObject
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition(..))
import Data.Array (foldl)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, const, map, (==))

data OptionName = Long String | Short Char

type OptionObject = Object OptionValue

data OptionValue
  = BooleanValue Boolean
  | StringValue String

defaults :: Array OptionDefinition -> OptionObject
defaults defs =
  foldl
    (\o d ->
      case d of
        BooleanOption { long } ->
          Object.alter (const (Just (BooleanValue false))) long o
        StringOption i ->
          case i.value of
            Nothing -> o
            Just v -> Object.alter (const (Just (StringValue v))) i.long o)
    Object.empty
    defs

findByOptionName :: OptionName -> Array OptionDefinition -> Maybe OptionDefinition
findByOptionName name =
  case name of
    Long l -> Array.find (\d -> getLongName d == l)
    Short s -> Array.find (\d -> getShortName d == Just s)
  where
  getLongName :: OptionDefinition -> String
  getLongName (BooleanOption { long }) = long
  getLongName (StringOption { long }) = long
  getShortName :: OptionDefinition -> Maybe Char
  getShortName (BooleanOption { short }) = short
  getShortName (StringOption { short }) = short

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

getBooleanValue :: String -> OptionObject -> Maybe Boolean
getBooleanValue n o = do
  value <- Object.lookup n o
  case value of
    BooleanValue b -> Just b
    StringValue _ -> Nothing

getStringValue :: String -> OptionObject -> Maybe String
getStringValue n o = do
  value <- Object.lookup n o
  case value of
    BooleanValue _ -> Nothing
    StringValue s -> Just s

-- ["--name", "value"] -> { name: "value" }
toObject :: Array OptionDefinition -> Array String -> OptionObject
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
                Just (BooleanOption { long }) ->
                  toObject'
                    (Array.drop 1 o)
                    (Object.insert long (BooleanValue true) p)
                Just (StringOption { long }) ->
                  case Array.head (Array.drop 1 o) of -- read metavar
                    Nothing -> p -- ERROR: no metavar (end)
                    Just value -> -- TODO: value == "--foo" -- no metavar (next option)
                      toObject'
                        (Array.drop 2 o)
                        (Object.insert long (StringValue value) p)
