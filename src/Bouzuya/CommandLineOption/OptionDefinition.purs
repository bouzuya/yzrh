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
  = OptionDefinition OptionType OptionInfo OptionInfo'

derive instance eqOptionDefinition :: Eq OptionDefinition

data OptionInfo
  = OptionInfo LongName (Maybe ShortName) Help Multiple

derive instance eqOptionInfo :: Eq OptionInfo

data OptionType
  = BooleanOption
  | StringOption

derive instance eqOptionType :: Eq OptionType

type ShortName = CodePoint

data TypedOptionDefinition a
  = TypedOptionDefinition OptionInfo (Maybe MetaVar) a

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    Nothing
    true

booleanOptionFromTyped :: TypedOptionDefinition Boolean -> OptionDefinition
booleanOptionFromTyped (TypedOptionDefinition info metavar _) =
  OptionDefinition
    BooleanOption
    info
    { metavar, value: Nothing }

booleanOption' :: BooleanOptionInfo -> NamedOptionDefinition
booleanOption' info =
  withName
    info.name
    (OptionDefinition
      BooleanOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false)
      { metavar: Nothing, value: Nothing })

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: NamedOptionDefinition -> Maybe (Array String)
getDefaultValue (NamedOptionDefinition _ (OptionDefinition BooleanOption _ _))
  = Nothing
getDefaultValue (NamedOptionDefinition _ (OptionDefinition StringOption _ { value }))
  = map Array.singleton value

getLongName :: NamedOptionDefinition -> String
getLongName (NamedOptionDefinition _ (OptionDefinition _ (OptionInfo long _ _ _) _)) = long

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getShortName :: NamedOptionDefinition -> Maybe Char
getShortName (NamedOptionDefinition _ (OptionDefinition _ (OptionInfo _ short _ _) _))
  = join (map charFromCodePoint short)

isValueMultiple :: NamedOptionDefinition -> Boolean
isValueMultiple (NamedOptionDefinition _ (OptionDefinition _ (OptionInfo _ _ _ b) _)) = b

isValueRequired :: NamedOptionDefinition -> Boolean
isValueRequired (NamedOptionDefinition _ (OptionDefinition BooleanOption _ _)) = false
isValueRequired (NamedOptionDefinition _ (OptionDefinition StringOption _ _)) = true

maybeStringOption :: LongName -> Maybe Char -> MetaVar -> Help -> Maybe String -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    (Just m)
    v

maybeStringOptionFromTyped :: TypedOptionDefinition (Maybe String) -> OptionDefinition
maybeStringOptionFromTyped (TypedOptionDefinition info metavar value) =
  OptionDefinition
    StringOption
    info
    { metavar, value }

stringOption :: LongName -> Maybe Char -> MetaVar -> Help -> String -> TypedOptionDefinition String
stringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h false)
    (Just m)
    v

stringOptionFromTyped :: TypedOptionDefinition String -> OptionDefinition
stringOptionFromTyped (TypedOptionDefinition info metavar value) =
  OptionDefinition
    StringOption
    info
    { metavar, value: Just value }

stringOption' :: StringOptionInfo -> NamedOptionDefinition
stringOption' info =
  withName
    info.name
    (OptionDefinition
      StringOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help
        false)
        { metavar: Just info.metavar, value: info.value })

withName :: String -> OptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
