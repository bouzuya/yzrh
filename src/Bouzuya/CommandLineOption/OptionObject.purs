module Bouzuya.CommandLineOption.OptionObject
  ( ParsedOption
  , getBooleanValue
  , getStringValue
  , parse
  ) where

import Bouzuya.CommandLineOption.OptionDefinition (NamedOptionDefinition, getDefaultValue, getLongName, getName, getShortName, isValueRequired)
import Data.Array (foldl)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (CodePoint)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, const, eq, map, not, pure, unit, (<<<), (==), (>>=))

data OptionName = Long String | Short CodePoint

type ParsedOption = { arguments :: Array String, options :: Object String }

addBooleanOptionValue :: NamedOptionDefinition -> Object String -> Object String
addBooleanOptionValue d o =
  Object.insert (getName d) "true" o

addStringOptionValue :: NamedOptionDefinition -> String -> Object String -> Object String
addStringOptionValue d v o =
  Object.insert (getName d) v o

defaultValues :: Array NamedOptionDefinition -> Object String
defaultValues defs =
  foldl
    (\o d -> Object.alter (const (getDefaultValue d)) (getName d) o)
    Object.empty
    defs

findByOptionName :: OptionName -> Array NamedOptionDefinition -> Maybe NamedOptionDefinition
findByOptionName (Long l) =
  Array.find (\d -> getLongName d == l)
findByOptionName (Short s) =
  Array.find (\d -> map String.codePointFromChar (getShortName d) == Just s)

getBooleanValue :: String -> Object String -> Maybe Boolean
getBooleanValue k o = Object.lookup k o >>= pure <<< eq "true"

getStringValue :: String -> Object String -> Maybe String
getStringValue = Object.lookup

parse :: Array NamedOptionDefinition -> Array String -> Either String ParsedOption
parse defs ss = do
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
      case parse' s of
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
      case parse' s of
        [] ->
          Right
            a
              { options = addStringOptionValue def s options
              , processing = Nothing
              }
        _ ->
          Left "no metavar (next)"

parse' :: String -> Array { name :: OptionName, value :: Maybe String }
parse' s =
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
