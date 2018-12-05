module CommandLineOption.OptionDefinition
  ( BooleanOptionInfo -- TODO: hide options
  , NamedOptionDefinition
  , StringOptionInfo -- TODO: hide options
  , TypedOptionDefinition
  , booleanOption
  , booleanOption'
  , getDefaultValue
  , getLongName
  , getName
  , getShortName
  , isValueRequired
  , maybeStringOption
  , stringOption
  , stringOption'
  ) where

import CommandLineOption.OptionValue (OptionValue)
import CommandLineOption.OptionValue as OptionValue
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Prelude (join, map)

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

type Name = String

data NamedOptionDefinition
  = NamedOptionDefinition Name UnNamedOptionDefinition

data OptionInfo
  = OptionInfo LongName (Maybe ShortName) Help

type ShortName = CodePoint

data TypedOptionDefinition a
  = TypedOptionDefinition OptionInfo (Maybe MetaVar) a

data UnNamedOptionDefinition
  = BooleanOption OptionInfo OptionInfo'
  | StringOption OptionInfo OptionInfo'

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h)
    Nothing
    true

booleanOption' :: BooleanOptionInfo -> NamedOptionDefinition
booleanOption' info =
  NamedOptionDefinition
    info.name
    (BooleanOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help)
      { metavar: Nothing, value: Nothing })

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: NamedOptionDefinition -> Maybe OptionValue
getDefaultValue (NamedOptionDefinition _ (BooleanOption _ _))
  = Just (OptionValue.fromBoolean false)
getDefaultValue (NamedOptionDefinition _ (StringOption _ { value }))
  = map OptionValue.fromString value

getLongName :: NamedOptionDefinition -> String
getLongName (NamedOptionDefinition _ (BooleanOption (OptionInfo long _ _) _)) = long
getLongName (NamedOptionDefinition _ (StringOption (OptionInfo long _ _) _)) = long

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getShortName :: NamedOptionDefinition -> Maybe Char
getShortName (NamedOptionDefinition _ (BooleanOption (OptionInfo _ short _) _))
  = join (map charFromCodePoint short)
getShortName (NamedOptionDefinition _ (StringOption (OptionInfo _ short _) _))
  = join (map charFromCodePoint short)

isValueRequired :: NamedOptionDefinition -> Boolean
isValueRequired (NamedOptionDefinition _ (BooleanOption _ _)) = false
isValueRequired (NamedOptionDefinition _ (StringOption _ _)) = true

maybeStringOption :: LongName -> Maybe Char -> MetaVar -> Help -> Maybe String -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h)
    (Just m)
    v

stringOption :: LongName -> Maybe Char -> MetaVar -> Help -> String -> TypedOptionDefinition String
stringOption l s m h v =
  TypedOptionDefinition
    (OptionInfo l (map String.codePointFromChar s) h)
    (Just m)
    v

stringOption' :: StringOptionInfo -> NamedOptionDefinition
stringOption' info =
  NamedOptionDefinition
    info.name
    (StringOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help)
        { metavar: Just info.metavar, value: info.value })
