module CommandLineOption.OptionObject
  ( OptionObject
  , toObject
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition, getDefaultValue, getLongName, getShortName, isValueRequired)
import CommandLineOption.OptionValue (OptionValue)
import CommandLineOption.OptionValue as OptionValue
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (CodePoint)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, const, map, not, unit, (==))

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
              map
                (\cp -> { name: Short cp, value: nullToNothing value })
                (String.toCodePointArray name)

toObject :: Array OptionDefinition -> Array String -> Either String OptionObject
toObject defs options = do
  { parsed, processing } <-
    foldl
      f
      (Right { parsed: defaultValues defs, processing: Nothing })
      options
  _ <- assert' "no metavar (end)" (isNothing processing)
  Right parsed
  where
    assert' s b = if b then Right unit else Left s
    f e@(Left _) _ = e
    f (Right { processing: Nothing, parsed }) s =
      case parseOption s of
        [] -> Left "arguments are not supported" -- TODO: add argument support
        [{ name, value: valueMaybe }] -> do
          -- TODO: long == "" -- double hyphen (--) support
          def <- note "unknown option" (findByOptionName name defs)
          case valueMaybe of
            Just value ->
              if isValueRequired def then
                Right
                  { parsed: Object.insert (getLongName def) (OptionValue.fromString value) parsed
                  , processing: Nothing
                  }
              else
                Left "boolean option can't specify value" -- TODO: add option name
            Nothing ->
              if isValueRequired def then
                Right
                  { parsed
                  , processing: Just def
                  }
              else
                Right
                  { parsed: Object.insert (getLongName def) (OptionValue.fromBoolean true) parsed
                  , processing: Nothing
                  }
        options' ->
          foldl
            (\e { name, value: valueMaybe } -> do
              { parsed: parsed' } <- e
              def <- note "unknown boolean option" (findByOptionName name defs) -- TODO: add option name
              _ <- assert' "-abc are boolean options" (not (isValueRequired def)) -- TODO: add option names
              _ <- assert' "-abc=val is invalid format" (isNothing valueMaybe) -- TODO: add option
              Right
                { parsed:
                    Object.insert
                      (getLongName def)
                      (OptionValue.fromBoolean true)
                      parsed'
                , processing: Nothing
                })
            (Right { parsed, processing: Nothing })
            options'
    f (Right { parsed, processing: Just def }) s =
      -- TODO: value == "--foo" -- no metavar (next option)
      Right
        { parsed: Object.insert (getLongName def) (OptionValue.fromString s) parsed
        , processing: Nothing
        }
