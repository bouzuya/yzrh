module Bouzuya.CommandLineOption.ObjectToRecord
  ( class OptionRecordBuilder
  , builder
  , toRecord
  ) where

import Bouzuya.CommandLineOption.OptionObject (OptionObject)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Data.Maybe (Maybe)
import Data.Symbol as Symbol
import Prelude (compose, identity, map, pure, (<$>), (<*>))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

class OptionRecordBuilder (list :: RowList) (from :: # Type) (to :: # Type)
  | list -> from to where
  builder :: RLProxy list -> OptionObject -> Maybe (Builder (Record from) (Record to))

instance optionRecordBuilderConsBoolean ::
  ( OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l Boolean from' to
  , Symbol.IsSymbol l
  ) => OptionRecordBuilder (Cons l Boolean t) from to where
  builder _ o = compose <$> (map (Builder.insert l) v) <*> (builder t o)
    where
      l = Symbol.SProxy :: Symbol.SProxy l
      k = Symbol.reflectSymbol l
      v = OptionObject.getBooleanValue k o
      t = RLProxy :: RLProxy t

instance optionRecordBuilderConsString ::
  ( OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l String from' to
  , Symbol.IsSymbol l
  ) => OptionRecordBuilder (Cons l String t) from to where
  builder _ o = compose <$> (map (Builder.insert l) v) <*> (builder t o)
    where
      l = Symbol.SProxy :: Symbol.SProxy l
      k = Symbol.reflectSymbol l
      v = OptionObject.getStringValue k o
      t = RLProxy :: RLProxy t

instance optionRecordBuilderConsMaybeString ::
  ( OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l (Maybe String) from' to
  , Symbol.IsSymbol l
  ) => OptionRecordBuilder (Cons l (Maybe String) t) from to where
  builder _ o = compose <$> pure (Builder.insert l v) <*> (builder t o)
    where
      l = Symbol.SProxy :: Symbol.SProxy l
      k = Symbol.reflectSymbol l
      v = OptionObject.getStringValue k o
      t = RLProxy :: RLProxy t

instance optionRecordBuilderNil :: OptionRecordBuilder Nil () () where
  builder _ _ = pure identity

toRecord :: forall rows list
   . RowToList rows list
  => OptionRecordBuilder list () rows
  => OptionObject
  -> Maybe (Record rows)
toRecord obj = Builder.build <$> (builder list obj) <*> (pure {})
  where
    list = RLProxy :: RLProxy list
