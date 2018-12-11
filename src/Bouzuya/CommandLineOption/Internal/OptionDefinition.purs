module Bouzuya.CommandLineOption.Internal.OptionDefinition
  ( BooleanOptionInfo -- TODO: delete
  , NamedOptionDefinition
  , OptionDefinition -- TODO
  , StringOptionInfo -- TODO: delete
  , TypedOptionDefinition
  , booleanOption
  , booleanOptionFromTyped
  , booleanOption' -- TODO: delete
  , getDefaultValue
  , getLongName
  , getName
  , getShortName
  , isValueMultiple
  , isValueRequired
  , maybeStringOption
  , maybeStringOptionFromTyped
  , stringOption
  , stringOptionFromTyped
  , stringOption' -- TODO: delete
  , withName -- TODO
  ) where

import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Prelude (class Eq, join, map)

type BooleanOptionInfo =
  { help :: String
  , long :: String
  , name :: String
  , short :: Maybe Char
  }

type StringOptionInfo =
  { help :: String
  , long :: String
  , metavar :: String
  , name :: String
  , short :: Maybe Char
  , value :: Maybe String
  }

type Help = String

type LongName = String

type MetaVar = String

type Multiple = Boolean

type Name = String

data NamedOptionDefinition
  = NamedOptionDefinition Name OptionDefinition

instance toElementBoolean ::
  RecordToArray.ToElement
    (TypedOptionDefinition Boolean)
    NamedOptionDefinition where
  toElement name a =
    withName name (booleanOptionFromTyped a)

instance toElementMaybeString ::
  RecordToArray.ToElement
    (TypedOptionDefinition (Maybe String))
    NamedOptionDefinition where
  toElement name a =
    withName name (maybeStringOptionFromTyped a)

instance toElementString ::
  RecordToArray.ToElement
    (TypedOptionDefinition String)
    NamedOptionDefinition where
  toElement name a =
    withName name (stringOptionFromTyped a)

derive instance eqNamedOptionDefinition :: Eq NamedOptionDefinition

data OptionDefinition
  = OptionDefinition
      LongName
      (Maybe ShortName)
      Help
      Multiple
      (Array MetaVar)
      (Maybe (Array String))

derive instance eqOptionDefinition :: Eq OptionDefinition

type ShortName = CodePoint

data TypedOptionDefinition a
  = TypedOptionDefinition OptionDefinition

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h = TypedOptionDefinition (option l s h [] Nothing)

booleanOptionFromTyped :: TypedOptionDefinition Boolean -> OptionDefinition
booleanOptionFromTyped (TypedOptionDefinition d) = d

booleanOption' :: BooleanOptionInfo -> NamedOptionDefinition
booleanOption' info =
  withName info.name (option info.long info.short info.help [] Nothing)

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: NamedOptionDefinition -> Maybe (Array String)
getDefaultValue (NamedOptionDefinition _ o) = getDefaultValue' o

getDefaultValue' :: OptionDefinition -> Maybe (Array String)
getDefaultValue' (OptionDefinition _ _ _ _ [] _) = Nothing
getDefaultValue' (OptionDefinition _ _ _ _ _ v) = v

getLongName :: NamedOptionDefinition -> String
getLongName (NamedOptionDefinition _ o) = getLongName' o

getLongName' :: OptionDefinition -> String
getLongName' (OptionDefinition long _ _ _ _ _) = long

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getShortName :: NamedOptionDefinition -> Maybe Char
getShortName (NamedOptionDefinition _ o) = getShortName' o

getShortName' :: OptionDefinition -> Maybe Char
getShortName' (OptionDefinition _ short _ _ _ _)
  = join (map charFromCodePoint short)

isValueMultiple :: NamedOptionDefinition -> Boolean
isValueMultiple (NamedOptionDefinition _ o) = isValueMultiple' o

isValueMultiple' :: OptionDefinition -> Boolean
isValueMultiple' (OptionDefinition _ _ _ b _ _) = b

isValueRequired :: NamedOptionDefinition -> Boolean
isValueRequired (NamedOptionDefinition _ o) = isValueRequired' o

isValueRequired' :: OptionDefinition -> Boolean
isValueRequired' (OptionDefinition _ _ _ _ [] _) = false
isValueRequired' (OptionDefinition _ _ _ _ _ _) = true

maybeStringOption :: LongName -> Maybe Char -> MetaVar -> Help -> Maybe String -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition (option l s h [m] (map Array.singleton v))

maybeStringOptionFromTyped :: TypedOptionDefinition (Maybe String) -> OptionDefinition
maybeStringOptionFromTyped (TypedOptionDefinition d) = d

option ::
  LongName
  -> Maybe Char
  -> Help
  -> Array MetaVar
  -> Maybe (Array String)
  -> OptionDefinition
option l s h m v =
  OptionDefinition l (map String.codePointFromChar s) h false m v

stringOption :: LongName -> Maybe Char -> MetaVar -> Help -> String -> TypedOptionDefinition String
stringOption l s m h v = TypedOptionDefinition (option l s h [m] (Just [v]))

stringOptionFromTyped :: TypedOptionDefinition String -> OptionDefinition
stringOptionFromTyped (TypedOptionDefinition d) = d

stringOption' :: StringOptionInfo -> NamedOptionDefinition
stringOption' info =
  withName
    info.name
    (option
      info.long
      info.short
      info.help
      [info.metavar]
      (map Array.singleton info.value))

withName :: String -> OptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
