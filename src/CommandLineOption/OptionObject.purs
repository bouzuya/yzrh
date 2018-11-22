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
import Data.String (CodePoint)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (const, map, (==))

data OptionName = Long String | Short CodePoint

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
    Short s -> Array.find (\d -> map String.codePointFromChar (getShortName d) == Just s)

parseOption :: String -> Array { name :: OptionName, value :: Maybe String }
parseOption s =
  let
    hyphen = String.Pattern "-"
    -- "foo=bar" -> { name: "foo", value: "bar" }
    split s' =
      case String.indexOf (String.Pattern "=") s' of
        Nothing -> { name: s', value: "" }
        Just index ->
          let { after, before } = String.splitAt index s'
          in { name: before, value: (String.drop 1 after) }
    nullToNothing s' = if String.null s' then Nothing else Just s' -- TODO: OK ?
  in
    case String.stripPrefix hyphen s of
      Nothing -> []
      Just s' ->
        case String.stripPrefix hyphen s' of
          Just s'' ->
            let { name, value } = split s''
            in [{ name: Long name, value: nullToNothing value }]
          _ ->
            let { name, value } = split s'
            in
              -- TODO: ERROR: -ab=foo
              map
                (\cp -> { name: Short cp, value: nullToNothing value })
                (String.toCodePointArray name)

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
      let
        options' = parseOption s
      in
        case options' of
          [] ->
            { parsed
            , processing: Nothing
            } -- ERROR: arguments is not supported
          [{ name, value: valueMaybe }] ->
            -- TODO: long == "" -- double hyphen (--) support
            case findByOptionName name defs of
              Nothing ->
                { parsed
                , processing: Nothing
                } -- ERROR: unknown option
              Just def ->
                case valueMaybe of
                  Just value ->
                    if isValueRequired def then
                      { parsed: Object.insert (getLongName def) (OptionValue.fromString value) parsed
                      , processing: Nothing
                      }
                    else
                      { parsed
                      , processing: Nothing
                      } -- ERROR: boolean option can't specify value.
                  Nothing ->
                    if isValueRequired def then
                      { parsed
                      , processing: Just def
                      }
                    else
                      { parsed: Object.insert (getLongName def) (OptionValue.fromBoolean true) parsed
                      , processing: Nothing
                      }
          _ ->
            { parsed:
                foldl
                  (\parsed' { name, value: valueMaybe } ->
                    case findByOptionName name defs of
                      Nothing -> parsed' -- ERROR: unknown option
                      Just def ->
                        case valueMaybe of
                          Just _ -> parsed' -- ERROR: -abc=val is invalid format
                          Nothing ->
                            if isValueRequired def then
                              parsed' -- ERROR: -abc are boolean options.
                            else
                              Object.insert
                                (getLongName def)
                                (OptionValue.fromBoolean true)
                                parsed')
                  parsed
                  options'
            , processing: Nothing
            }

    f { parsed, processing: Just def } s =
      -- TODO: value == "--foo" -- no metavar (next option)
      { parsed: Object.insert (getLongName def) (OptionValue.fromString s) parsed
      , processing: Nothing
      }
