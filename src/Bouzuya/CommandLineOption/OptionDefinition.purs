module Bouzuya.CommandLineOption.OptionDefinition
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

type OptionInfo' =
  { metavar :: Maybe String
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
  = BooleanOption OptionInfo
  | StringOption OptionInfo

derive instance eqOptionDefinition :: Eq OptionDefinition

data OptionInfo
  = OptionInfo LongName (Maybe ShortName) Help Multiple (Maybe MetaVar) (Maybe String)

derive instance eqOptionInfo :: Eq OptionInfo

type ShortName = CodePoint

data TypedOptionDefinition a
  = TypedOptionDefinition OptionInfo a

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false Nothing Nothing)
    true

booleanOptionFromTyped :: TypedOptionDefinition Boolean -> OptionDefinition
booleanOptionFromTyped (TypedOptionDefinition (OptionInfo l s h m v _) _) =
  BooleanOption (OptionInfo l s h m v Nothing)

booleanOption' :: BooleanOptionInfo -> NamedOptionDefinition
booleanOption' info =
  withName
    info.name
    (BooleanOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false
        Nothing
        Nothing))

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: NamedOptionDefinition -> Maybe (Array String)
getDefaultValue (NamedOptionDefinition _ (BooleanOption _)) = Nothing
getDefaultValue (NamedOptionDefinition _ (StringOption (OptionInfo _ _ _ _ _ value)))
  = map Array.singleton value

getLongName :: NamedOptionDefinition -> String
getLongName (NamedOptionDefinition _ (BooleanOption (OptionInfo long _ _ _ _ _))) = long
getLongName (NamedOptionDefinition _ (StringOption (OptionInfo long _ _ _ _ _))) = long

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getShortName :: NamedOptionDefinition -> Maybe Char
getShortName (NamedOptionDefinition _ (BooleanOption (OptionInfo _ short _ _ _ _)))
  = join (map charFromCodePoint short)
getShortName (NamedOptionDefinition _ (StringOption (OptionInfo _ short _ _ _ _)))
  = join (map charFromCodePoint short)

isValueMultiple :: NamedOptionDefinition -> Boolean
isValueMultiple (NamedOptionDefinition _ (BooleanOption (OptionInfo _ _ _ b _ _))) = b
isValueMultiple (NamedOptionDefinition _ (StringOption (OptionInfo _ _ _ b _ _))) = b

isValueRequired :: NamedOptionDefinition -> Boolean
isValueRequired (NamedOptionDefinition _ (BooleanOption _)) = false
isValueRequired (NamedOptionDefinition _ (StringOption _)) = true

maybeStringOption :: LongName -> Maybe Char -> MetaVar -> Help -> Maybe String -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false (Just m) v)
    v

maybeStringOptionFromTyped :: TypedOptionDefinition (Maybe String) -> OptionDefinition
maybeStringOptionFromTyped (TypedOptionDefinition info _) =
  StringOption info

stringOption :: LongName -> Maybe Char -> MetaVar -> Help -> String -> TypedOptionDefinition String
stringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false (Just m) (Just v))
    v

stringOptionFromTyped :: TypedOptionDefinition String -> OptionDefinition
stringOptionFromTyped (TypedOptionDefinition (OptionInfo l s h m v _) value) =
  StringOption (OptionInfo l s h m v (Just value))

stringOption' :: StringOptionInfo -> NamedOptionDefinition
stringOption' info =
  withName
    info.name
    (StringOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false
        (Just info.metavar)
        info.value))

withName :: String -> OptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
