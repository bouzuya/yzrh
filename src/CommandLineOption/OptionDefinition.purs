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
import Prelude (map)

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

data OptionDefinition
  = BooleanOption BooleanOptionInfo
  | StringOption StringOptionInfo

booleanOption :: BooleanOptionInfo -> OptionDefinition
booleanOption = BooleanOption

getDefaultValue :: OptionDefinition -> Maybe OptionValue
getDefaultValue (BooleanOption _) = Just (OptionValue.fromBoolean false)
getDefaultValue (StringOption { value }) = map OptionValue.fromString value

getLongName :: OptionDefinition -> String
getLongName (BooleanOption { long }) = long
getLongName (StringOption { long }) = long

getShortName :: OptionDefinition -> Maybe Char
getShortName (BooleanOption { short }) = short
getShortName (StringOption { short }) = short

isValueRequired :: OptionDefinition -> Boolean
isValueRequired (BooleanOption _) = false
isValueRequired (StringOption _) = true

stringOption :: StringOptionInfo -> OptionDefinition
stringOption = StringOption
