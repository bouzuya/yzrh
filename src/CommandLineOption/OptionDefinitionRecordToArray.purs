module CommandLineOption.OptionDefinitionRecordToArray
  ( class OptionArrayBuilder
  , builder
  , toArray
  ) where

import CommandLineOption.OptionDefinition (NamedOptionDefinition, TypedOptionDefinition, booleanOptionFromTyped, maybeStringOptionFromTyped, stringOptionFromTyped, withName)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Data.RowList (RLProxy(..))

class OptionArrayBuilder (list :: RowList) (from :: # Type) (to :: # Type)
  | list -> from to where
  builder :: RLProxy list -> Record to -> Array NamedOptionDefinition -> Array NamedOptionDefinition

instance optionArrayBuilderConsBoolean ::
  ( IsSymbol l
  , OptionArrayBuilder t from from'
  , Row.Cons l (TypedOptionDefinition Boolean) from' to
  , Row.Lacks l from'
  ) => OptionArrayBuilder (Cons l (TypedOptionDefinition Boolean) t) from to where
  builder _ o a =
    builder
      t
      (Record.delete l o)
      (Array.snoc a (withName k (booleanOptionFromTyped v)))
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = Record.get l o
      t = RLProxy :: RLProxy t

instance optionArrayBuilderConsMaybeString ::
  ( IsSymbol l
  , OptionArrayBuilder t from from'
  , Row.Cons l (TypedOptionDefinition (Maybe String)) from' to
  , Row.Lacks l from'
  ) => OptionArrayBuilder (Cons l (TypedOptionDefinition (Maybe String)) t) from to where
  builder _ o a =
    builder
      t
      (Record.delete l o)
      (Array.snoc a (withName k (maybeStringOptionFromTyped v)))
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = Record.get l o
      t = RLProxy :: RLProxy t

instance optionArrayBuilderConsString ::
  ( IsSymbol l
  , OptionArrayBuilder t from from'
  , Row.Cons l (TypedOptionDefinition String) from' to
  , Row.Lacks l from'
  ) => OptionArrayBuilder (Cons l (TypedOptionDefinition String) t) from to where
  builder _ o a =
    builder
      t
      (Record.delete l o)
      (Array.snoc a (withName k (stringOptionFromTyped v)))
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = Record.get l o
      t = RLProxy :: RLProxy t

instance optionArrayBuilderNil :: OptionArrayBuilder Nil () () where
  builder _ _ a = a

toArray :: forall rows list
   . RowToList rows list
  => OptionArrayBuilder list () rows
  => Record rows
  -> Array NamedOptionDefinition
toArray obj = builder list obj []
  where
    list = RLProxy :: RLProxy list
