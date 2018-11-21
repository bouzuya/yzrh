module CommandLineOption.OptionDefinition
  ( BooleanOptionInfo
  , OptionDefinition(..)
  , StringOptionInfo
  ) where

import Data.Maybe (Maybe)

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

type OptionDefinitions = Array OptionDefinition
