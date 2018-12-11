module Bouzuya.CommandLineOption.TypedOptionDefinition
  ( TypedOptionDefinition
  , booleanOption
  , fromTyped -- private
  , maybeStringOption
  , stringOption
  ) where

import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition, withName)
import Bouzuya.CommandLineOption.OptionDefinition (Help, LongName, OptionDefinition, MetaVar, option)
import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Prelude (map)

instance toElementBoolean ::
  RecordToArray.ToElement
    (TypedOptionDefinition Boolean)
    NamedOptionDefinition where
  toElement name a = withName name (fromTyped a)

instance toElementMaybeString ::
  RecordToArray.ToElement
    (TypedOptionDefinition (Maybe String))
    NamedOptionDefinition where
  toElement name a = withName name (fromTyped a)

instance toElementString ::
  RecordToArray.ToElement
    (TypedOptionDefinition String)
    NamedOptionDefinition where
  toElement name a = withName name (fromTyped a)

data TypedOptionDefinition a
  = TypedOptionDefinition OptionDefinition

booleanOption :: LongName -> Maybe Char -> Help -> TypedOptionDefinition Boolean
booleanOption l s h = TypedOptionDefinition (option l s h [] Nothing)

fromTyped :: forall a. TypedOptionDefinition a -> OptionDefinition
fromTyped (TypedOptionDefinition d) = d

maybeStringOption ::
  LongName
  -> Maybe Char
  -> MetaVar
  -> Help
  -> Maybe String
  -> TypedOptionDefinition (Maybe String)
maybeStringOption l s m h v =
  TypedOptionDefinition (option l s h [m] (map Array.singleton v))

stringOption ::
  LongName
  -> Maybe Char
  -> MetaVar
  -> Help
  -> String
  -> TypedOptionDefinition String
stringOption l s m h v = TypedOptionDefinition (option l s h [m] (Just [v]))
