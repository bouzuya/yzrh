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
  = OptionDefinition Name UnNamedOptionDefinition

data OptionInfo
  = OptionInfo LongName (Maybe ShortName) Help

data UnNamedOptionDefinition
  = BooleanOption OptionInfo
  | StringOption OptionInfo StringOptionInfo'

type ShortName = CodePoint

booleanOption :: BooleanOptionInfo -> OptionDefinition
booleanOption info =
  OptionDefinition
    info.name
    (BooleanOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help))

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: OptionDefinition -> Maybe OptionValue
getDefaultValue (OptionDefinition _ (BooleanOption _))
  = Just (OptionValue.fromBoolean false)
getDefaultValue (OptionDefinition _ (StringOption _ { value }))
  = map OptionValue.fromString value

getLongName :: OptionDefinition -> String
getLongName (OptionDefinition _ (BooleanOption (OptionInfo long _ _))) = long
getLongName (OptionDefinition _ (StringOption (OptionInfo long _ _) _)) = long

getName :: OptionDefinition -> String
getName (OptionDefinition name _) = name

getShortName :: OptionDefinition -> Maybe Char
getShortName (OptionDefinition _ (BooleanOption (OptionInfo _ short _)))
  = join (map charFromCodePoint short)
getShortName (OptionDefinition _ (StringOption (OptionInfo _ short _) _))
  = join (map charFromCodePoint short)

isValueRequired :: OptionDefinition -> Boolean
isValueRequired (OptionDefinition _ (BooleanOption _)) = false
isValueRequired (OptionDefinition _ (StringOption _ _)) = true

stringOption :: StringOptionInfo -> OptionDefinition
stringOption info =
  OptionDefinition
    info.name
    (StringOption
      (OptionInfo
        info.long
        (map String.codePointFromChar info.short)
        info.help)
        { metavar: info.metavar, value: info.value })
