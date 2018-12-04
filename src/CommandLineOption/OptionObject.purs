module CommandLineOption.OptionObject
  ( OptionObject
  , toObject
  ) where

import CommandLineOption.OptionDefinition (OptionDefinition, getDefaultValue, getLongName, getName, getShortName, isValueRequired)
import CommandLineOption.OptionValue (OptionValue)
import CommandLineOption.OptionValue as OptionValue
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (CodePoint)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, const, map, not, unit, (==))

data OptionName = Long String | Short CodePoint

type OptionObject = Object OptionValue

addBooleanOptionValue :: OptionDefinition -> OptionObject -> OptionObject
addBooleanOptionValue d o =
  Object.insert (getName d) (OptionValue.fromBoolean true) o

addStringOptionValue :: OptionDefinition -> String -> OptionObject -> OptionObject
addStringOptionValue d v o =
  Object.insert (getName d) (OptionValue.fromString v) o

defaultValues :: Array OptionDefinition -> OptionObject
defaultValues defs =
  foldl
    (\o d -> Object.alter (const (getDefaultValue d)) (getName d) o)
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
    -- "foo" -> { name: "foo", value: Nothing }
    -- "foo=" -> { name: "foo", value: Just "" }
    -- "foo=bar" -> { name: "foo", value: Just "bar" }
    split s' =
      case String.indexOf (String.Pattern "=") s' of
        Nothing -> { name: s', value: Nothing }
        Just index ->
          let { after, before } = String.splitAt index s'
          in { name: before, value: Just (String.drop 1 after) }
  in
    case String.stripPrefix hyphen s of
      Nothing -> []
      Just s' ->
        case String.stripPrefix hyphen s' of
          Just s'' ->
            let { name, value } = split s''
            in [{ name: Long name, value }]
          _ ->
            let { name, value } = split s'
            in
              map
                (\cp -> { name: Short cp, value })
                (String.toCodePointArray name)

toObject :: Array OptionDefinition -> Array String -> Either String { arguments :: Array String, options :: OptionObject }
toObject defs ss = do
  { arguments, options, processing } <-
    foldM
      f
      { arguments: [], options: defaultValues defs, processing: Nothing }
      ss
  _ <- assert' "no metavar (end)" (isNothing processing)
  Right { arguments, options }
  where
    assert' s b = if b then Right unit else Left s
    f a@{ arguments, options, processing: Nothing } s =
      case parseOption s of
        [] -> -- foo
          Right a { arguments = Array.snoc arguments s }
        [{ name, value: valueMaybe }] -> do -- -a or --abc
          -- TODO: long == "" -- double hyphen (--) support
          _ <- assert' "invalid option position" (Array.null arguments)
          def <- note "unknown option" (findByOptionName name defs)
          case valueMaybe of
            Just value ->
              if isValueRequired def then
                Right a { options = addStringOptionValue def value options }
              else
                Left "boolean option can't specify value" -- TODO: add option name
            Nothing ->
              if isValueRequired def then
                Right a { processing = Just def }
              else
                Right a { options = addBooleanOptionValue def options }
        shortOptions -> do -- -abc
          _ <- assert' "invalid option position" (Array.null arguments)
          foldM
            (\a'@{ options: options' } { name, value: valueMaybe } -> do
              def <- note "unknown boolean option" (findByOptionName name defs) -- TODO: add option name
              _ <- assert' "-abc are boolean options" (not (isValueRequired def)) -- TODO: add option names
              _ <- assert' "-abc=val is invalid format" (isNothing valueMaybe) -- TODO: add option
              Right a' { options = addBooleanOptionValue def options' })
            a
            shortOptions
    f a@{ options, processing: Just def } s =
      case parseOption s of
        [] ->
          Right
            a
              { options = addStringOptionValue def s options
              , processing = Nothing
              }
        _ ->
          Left "no metavar (next)"
