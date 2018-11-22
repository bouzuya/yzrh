module CommandLineOption.OptionDefinition
  ( BooleanOptionInfo -- TODO: hide options
  , OptionDefinition
  , StringOptionInfo -- TODO: hide options
  , booleanOption
  , getDefaultValue
  , getLongName
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
  , short :: Maybe Char
  }

type StringOptionInfo =
  { help :: String
  , long :: String
  , metavar :: String
  , short :: Maybe Char
  , value :: Maybe String
  }

type StringOptionInfo' =
  { metavar :: String
  , value :: Maybe String
  }

type Help = String

type LongName = String

data OptionDefinition
  = BooleanOption LongName (Maybe ShortName) Help
  | StringOption LongName (Maybe ShortName) Help StringOptionInfo'

type ShortName = CodePoint

booleanOption :: BooleanOptionInfo -> OptionDefinition
booleanOption info =
  BooleanOption
    info.long
    (map String.codePointFromChar info.short)
    info.help

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: OptionDefinition -> Maybe OptionValue
getDefaultValue (BooleanOption _ _ _) = Just (OptionValue.fromBoolean false)
getDefaultValue (StringOption _ _ _ { value }) = map OptionValue.fromString value

getLongName :: OptionDefinition -> String
getLongName (BooleanOption long _ _) = long
getLongName (StringOption long _ _ _) = long

getShortName :: OptionDefinition -> Maybe Char
getShortName (BooleanOption _ short _) = join (map charFromCodePoint short)
getShortName (StringOption _ short _ _) = join (map charFromCodePoint short)

isValueRequired :: OptionDefinition -> Boolean
isValueRequired (BooleanOption _ _ _) = false
isValueRequired (StringOption _ _ _ _) = true

stringOption :: StringOptionInfo -> OptionDefinition
stringOption info =
  StringOption
    info.long
    (map String.codePointFromChar info.short)
    info.help
    { metavar: info.metavar, value: info.value }
