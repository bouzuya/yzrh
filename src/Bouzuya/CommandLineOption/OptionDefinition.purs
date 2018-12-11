module Bouzuya.CommandLineOption.OptionDefinition
  ( Help
  , LongName
  , MetaVar
  , Multiple
  , OptionDefinition
  , ShortName
  , getDefaultValue
  , getLongName
  , getShortName
  , isValueMultiple
  , isValueRequired
  , option
  ) where

import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.String as String
import Data.String.CodeUnits as CodeUnit
import Prelude (class Eq, join, map)

type Help = String

type LongName = String

type MetaVar = String

type Multiple = Boolean

data OptionDefinition
  = OptionDefinition
      LongName
      (Maybe ShortName)
      Help
      Multiple
      (Array MetaVar)
      (Maybe (Array String))

derive instance eqOptionDefinition :: Eq OptionDefinition

type ShortName = CodePoint

charFromCodePoint :: CodePoint -> Maybe Char
charFromCodePoint cp = CodeUnit.charAt 0 (String.singleton cp)

getDefaultValue :: OptionDefinition -> Maybe (Array String)
getDefaultValue (OptionDefinition _ _ _ _ [] _) = Nothing
getDefaultValue (OptionDefinition _ _ _ _ _ v) = v

getLongName :: OptionDefinition -> String
getLongName (OptionDefinition long _ _ _ _ _) = long

getShortName :: OptionDefinition -> Maybe Char
getShortName (OptionDefinition _ short _ _ _ _)
  = join (map charFromCodePoint short)

isValueMultiple :: OptionDefinition -> Boolean
isValueMultiple (OptionDefinition _ _ _ b _ _) = b

isValueRequired :: OptionDefinition -> Boolean
isValueRequired (OptionDefinition _ _ _ _ [] _) = false
isValueRequired (OptionDefinition _ _ _ _ _ _) = true

option ::
  LongName
  -> Maybe Char
  -> Help
  -> Array MetaVar
  -> Maybe (Array String)
  -> OptionDefinition
option l s h m v =
  OptionDefinition l (map String.codePointFromChar s) h false m v
