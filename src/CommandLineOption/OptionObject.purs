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
import Prelude (const, map, notEq, (<<<), (==))

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
    v = String.takeWhile (notEq (String.codePointFromChar '='))
  in
    case String.stripPrefix p s of
      Just s' ->
        case String.stripPrefix p s' of
          Just s'' -> Just (Long (v s''))
          _ -> map Short (CodeUnit.charAt 0 (v s')) -- TODO: -fg
      Nothing -> Nothing

getOptionValue :: String -> Maybe String
getOptionValue s =
  let
    p = String.Pattern "-"
    v = (String.drop 1) <<< (String.dropWhile (notEq (String.codePointFromChar '=')))
  in
    case String.stripPrefix p s of
      Just s' ->
        let s'' = v s'
        in if String.null s'' then Nothing else Just s''
      Nothing -> Nothing

-- ["--name", "value"] -> { name: "value" }
toObject :: Array OptionDefinition -> Array String -> OptionObject
toObject defs options =
  let
    parsed =
      foldl
        f
        { parsed: defaultValues defs, processing: Nothing }
        options
  in
    case parsed.processing of
      Nothing -> parsed.parsed
      Just _ -> parsed.parsed -- ERROR: no metavar (end)
  where
    f { processing: Nothing, parsed } s =
      case getOptionName s of
        Nothing ->
          { parsed
          , processing: Nothing
          } -- ERROR: arguments is not supported
        Just name ->
          -- TODO: long == "" -- double hyphen (--) support
          case findByOptionName name defs of
            Nothing ->
              { parsed
              , processing: Nothing
              } -- ERROR: unknown option
            Just def ->
              let
                long = getLongName def
              in
                if isValueRequired def then
                  case getOptionValue s of
                    Just value ->
                      { parsed: Object.insert long (OptionValue.fromString value) parsed
                      , processing: Nothing
                      }
                    Nothing ->
                      { parsed
                      , processing: Just def
                      }
                else
                  { parsed: Object.insert long (OptionValue.fromBoolean true) parsed
                  , processing: Nothing
                  }
    f { parsed, processing: Just def } s =
      -- TODO: value == "--foo" -- no metavar (next option)
      { parsed: Object.insert (getLongName def) (OptionValue.fromString s) parsed
      , processing: Nothing
      }
