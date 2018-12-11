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
  = OptionDefinition OptionInfo OptionType (Maybe String)

derive instance eqOptionDefinition :: Eq OptionDefinition

data OptionInfo
  = OptionInfo LongName (Maybe ShortName) Help Multiple

derive instance eqOptionInfo :: Eq OptionInfo

data OptionType
  = BooleanOption
  | StringOption MetaVar

derive instance eqOptionType :: Eq OptionType

type ShortName = CodePoint

data TypedOptionDefinition a
  = TypedOptionDefinition OptionInfo OptionType a

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    BooleanOption
    true

booleanOptionFromTyped :: TypedOptionDefinition Boolean -> OptionDefinition
booleanOptionFromTyped (TypedOptionDefinition info optionType _) =
  OptionDefinition info optionType Nothing

booleanOption' :: BooleanOptionInfo -> NamedOptionDefinition
booleanOption' info =
  withName
    info.name
    (OptionDefinition
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false)
      BooleanOption
      Nothing)

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: NamedOptionDefinition -> Maybe (Array String)
getDefaultValue (NamedOptionDefinition _ o) = getDefaultValue' o

getDefaultValue' :: OptionDefinition -> Maybe (Array String)
getDefaultValue' (OptionDefinition _ BooleanOption _) = Nothing
getDefaultValue' (OptionDefinition _ (StringOption _) value)
  = map Array.singleton value

getLongName :: NamedOptionDefinition -> String
getLongName (NamedOptionDefinition _ o) = getLongName' o

getLongName' :: OptionDefinition -> String
getLongName' (OptionDefinition (OptionInfo long _ _ _) _ _) = long

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getShortName :: NamedOptionDefinition -> Maybe Char
getShortName (NamedOptionDefinition _ o) = getShortName' o

getShortName' :: OptionDefinition -> Maybe Char
getShortName' (OptionDefinition (OptionInfo _ short _ _) _ _)
  = join (map charFromCodePoint short)

isValueMultiple :: NamedOptionDefinition -> Boolean
isValueMultiple (NamedOptionDefinition _ o) = isValueMultiple' o

isValueMultiple' :: OptionDefinition -> Boolean
isValueMultiple' (OptionDefinition (OptionInfo _ _ _ b) _ _) = b

isValueRequired :: NamedOptionDefinition -> Boolean
isValueRequired (NamedOptionDefinition _ o) = isValueRequired' o

isValueRequired' :: OptionDefinition -> Boolean
isValueRequired' (OptionDefinition _ BooleanOption _) = false
isValueRequired' (OptionDefinition _ (StringOption _) _) = true

maybeStringOption :: LongName -> Maybe Char -> MetaVar -> Help -> Maybe String -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    (StringOption m)
    v

maybeStringOptionFromTyped :: TypedOptionDefinition (Maybe String) -> OptionDefinition
maybeStringOptionFromTyped (TypedOptionDefinition info optionType value) =
  OptionDefinition info optionType value

stringOption :: LongName -> Maybe Char -> MetaVar -> Help -> String -> TypedOptionDefinition String
stringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    (StringOption m)
    v

stringOptionFromTyped :: TypedOptionDefinition String -> OptionDefinition
stringOptionFromTyped (TypedOptionDefinition info optionType value) =
  OptionDefinition info optionType (Just value)

stringOption' :: StringOptionInfo -> NamedOptionDefinition
stringOption' info =
  withName
    info.name
    (OptionDefinition
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false)
      (StringOption info.metavar)
      info.value)

withName :: String -> OptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
