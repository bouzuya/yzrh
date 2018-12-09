module Bouzuya.CommandLineOption.OptionObject
  ( OptionObject
  , ParsedOption
  , fromFoldable -- TODO: remove
  , getBooleanValue
  , getStringValue
  , merge -- TODO: remove
  , parse
  ) where

import Bouzuya.CommandLineOption.OptionDefinition (NamedOptionDefinition, getDefaultValue, getLongName, getName, getShortName, isValueRequired)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, foldl, foldM, foldlDefault)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (CodePoint)
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (class Eq, class Functor, class Show, bind, const, map, not, pure, show, unit, (<>), (==))

data OptionName = Long String | Short CodePoint

newtype OptionObject = OptionObject (Object (Array String))

derive instance eqOptionObject :: Eq OptionObject

instance showOptionObject :: Show OptionObject where
  show (OptionObject o) = "(OptionObject " <> show o <> ")"

type ParsedOption = { arguments :: Array String, options :: OptionObject }

addBooleanOptionValue :: NamedOptionDefinition -> OptionObject -> OptionObject
addBooleanOptionValue d (OptionObject o) =
  OptionObject
    (Object.alter
      (\m -> Just ((fromMaybe [] m) <> ["true"])) -- TODO
      (getName d)
      o)

addStringOptionValue :: NamedOptionDefinition -> String -> OptionObject -> OptionObject
addStringOptionValue d v (OptionObject o) =
  OptionObject
    (Object.alter
      (\m -> Just ((fromMaybe [] m) <> [v]))
      (getName d)
      o)

defaultValues :: Array NamedOptionDefinition -> OptionObject
defaultValues defs =
  OptionObject
    (foldlDefault
      (\o d -> Object.alter (const (map Array.singleton (getDefaultValue d))) (getName d) o)
      Object.empty
      defs)

findByOptionName :: OptionName -> Array NamedOptionDefinition -> Maybe NamedOptionDefinition
findByOptionName (Long l) =
  Array.find (\d -> getLongName d == l)
findByOptionName (Short s) =
  Array.find (\d -> map String.codePointFromChar (getShortName d) == Just s)

-- TODO: remove
fromFoldable ::
  forall f
   . Foldable f
  => Functor f
  => f (Tuple String String)
  -> OptionObject
fromFoldable f =
  OptionObject (Object.fromFoldable (map g f))
  where
    g (Tuple k v) = Tuple k (Array.singleton v)

getBooleanValue :: String -> OptionObject -> Maybe Boolean
getBooleanValue k (OptionObject o) = do
  a <- Object.lookup k o
  v <- Array.head a
  pure (v == "true") -- TODO

getStringValue :: String -> OptionObject -> Maybe String
getStringValue k (OptionObject o) = do
  a <- Object.lookup k o
  v <- Array.head a
  pure v

parse :: Array NamedOptionDefinition -> Array String -> Either String ParsedOption
parse defs ss = do
  { arguments, options, processing } <-
    foldM
      f
      { arguments: [], options: OptionObject Object.empty, processing: Nothing }
      ss
  _ <- assert' "no metavar (end)" (isNothing processing)
  Right { arguments, options: merge options (defaultValues defs) }
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

-- TODO: remove
merge :: OptionObject -> OptionObject -> OptionObject
merge (OptionObject o1) (OptionObject o2) =
  OptionObject
    (foldl
      (\o k ->
        case Object.lookup k o1 of
          Nothing -> o
          Just v -> Object.insert k v o)
      o2
      (Object.keys o1))
