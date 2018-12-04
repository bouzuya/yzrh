module CommandLineOption.OptionDefinition
  ( BooleanOptionInfo -- TODO: hide options
  , OptionDefinition
  , StringOptionInfo -- TODO: hide options
  , booleanOption
  , getDefaultValue
  , getLongName
  , getName
  , getShortName
  , isValueRequired
  , stringOption
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

type StringOptionInfo' =
  { metavar :: String
  , value :: Maybe String
  }

type Help = String

type LongName = String

type Name = String

data OptionDefinition
  = BooleanOption Name LongName (Maybe ShortName) Help
  | StringOption Name LongName (Maybe ShortName) Help StringOptionInfo'

type ShortName = CodePoint

booleanOption :: BooleanOptionInfo -> OptionDefinition
booleanOption info =
  BooleanOption
    info.name
    info.long
    (map String.codePointFromChar info.short)
    info.help

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: OptionDefinition -> Maybe OptionValue
getDefaultValue (BooleanOption _ _ _ _) = Just (OptionValue.fromBoolean false)
getDefaultValue (StringOption _ _ _ _ { value }) = map OptionValue.fromString value

getLongName :: OptionDefinition -> String
getLongName (BooleanOption _ long _ _) = long
getLongName (StringOption _ long _ _ _) = long

getName :: OptionDefinition -> String
getName (BooleanOption name _ _ _) = name
getName (StringOption name _ _ _ _) = name

getShortName :: OptionDefinition -> Maybe Char
getShortName (BooleanOption _ _ short _) = join (map charFromCodePoint short)
getShortName (StringOption _ _ short _ _) = join (map charFromCodePoint short)

isValueRequired :: OptionDefinition -> Boolean
isValueRequired (BooleanOption _ _ _ _) = false
isValueRequired (StringOption _ _ _ _ _) = true

stringOption :: StringOptionInfo -> OptionDefinition
stringOption info =
  StringOption
    info.name
    info.long
    (map String.codePointFromChar info.short)
    info.help
    { metavar: info.metavar, value: info.value }
