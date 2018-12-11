module Bouzuya.CommandLineOption.NamedOptionDefinition
  ( NamedOptionDefinition
  , getName
  , getOption
  , withName
  ) where

import Bouzuya.CommandLineOption.OptionDefinition (OptionDefinition)
import Prelude (class Eq)

type Name = String

data NamedOptionDefinition
  = NamedOptionDefinition Name OptionDefinition

derive instance eqNamedOptionDefinition :: Eq NamedOptionDefinition

getName :: NamedOptionDefinition -> String
getName (NamedOptionDefinition name _) = name

getOption :: NamedOptionDefinition -> OptionDefinition
getOption (NamedOptionDefinition _ o) = o

withName :: String -> OptionDefinition -> NamedOptionDefinition
withName = NamedOptionDefinition
