module CommandLineOption.OptionDefinitionRecordToArray
  ( class OptionArrayBuilder
  , class ToElement
  , builder
  , toArray
  , toElement
  ) where

import CommandLineOption.OptionDefinition (NamedOptionDefinition, TypedOptionDefinition, booleanOptionFromTyped, maybeStringOptionFromTyped, stringOptionFromTyped, withName)
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Data.RowList (RLProxy(..))

class ToElement (from :: Type) (to :: Type) | from -> to where
  toElement :: String -> from -> to

instance toElementBoolean ::
  ToElement (TypedOptionDefinition Boolean) NamedOptionDefinition where
  toElement name a =
    withName name (booleanOptionFromTyped a)

instance toElementMaybeString ::
  ToElement (TypedOptionDefinition (Maybe String)) NamedOptionDefinition where
  toElement name a =
    withName name (maybeStringOptionFromTyped a)

instance toElementString ::
  ToElement (TypedOptionDefinition String) NamedOptionDefinition where
  toElement name a =
    withName name (stringOptionFromTyped a)

class OptionArrayBuilder (list :: RowList) (from :: # Type) (to :: # Type) (a :: Type)
  | list -> from to where
  builder :: RLProxy list -> Record to -> Array a -> Array a

instance optionArrayBuilderConsBoolean ::
  ( IsSymbol l
  , OptionArrayBuilder t from from' ty'
  , Row.Cons l ty from' to
  , Row.Lacks l from'
  , ToElement ty ty'
  ) => OptionArrayBuilder (Cons l ty t) from to ty' where
  builder _ o a =
    builder
      t
      (Record.delete l o)
      (Array.snoc a (toElement k v))
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = Record.get l o
      t = RLProxy :: RLProxy t

instance optionArrayBuilderNil :: OptionArrayBuilder Nil () () ty' where
  builder _ _ a = a

toArray :: forall a b rows list
   . ToElement a b
  => RowToList rows list
  => OptionArrayBuilder list () rows b
  => Record rows
  -> Array b
toArray obj = builder list obj []
  where
    list = RLProxy :: RLProxy list
