module ObjectToRecord
  ( class OptionRecordBuilder
  , builder
  , toRecord
  ) where

import CommandLineOption.OptionObject (OptionObject)
import CommandLineOption.OptionValue (OptionValue)
import CommandLineOption.OptionValue as OptionValue
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (compose, identity, join, map, pure, (<$>), (<*>))
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

getBooleanValue :: String -> Object OptionValue -> Maybe Boolean
getBooleanValue k o = join (map OptionValue.getBooleanValue (Object.lookup k o))

getStringValue :: String -> Object OptionValue -> Maybe String
getStringValue k o = join (map OptionValue.getStringValue (Object.lookup k o))

class OptionRecordBuilder (list :: RowList) (from :: # Type) (to :: # Type)
  | list -> from to where
  builder :: RLProxy list -> Object OptionValue -> Maybe (Builder (Record from) (Record to))

instance optionRecordBuilderConsBoolean ::
  ( IsSymbol l
  , OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l Boolean from' to
  ) => OptionRecordBuilder (Cons l Boolean t) from to where
  builder _ o = compose <$> (map (Builder.insert l) v) <*> (builder t o)
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = getBooleanValue k o
      t = RLProxy :: RLProxy t

instance optionRecordBuilderConsString ::
  ( IsSymbol l
  , OptionRecordBuilder t from from'
  , Row.Lacks l from'
  , Row.Cons l String from' to
  ) => OptionRecordBuilder (Cons l String t) from to where
  builder _ o = compose <$> (map (Builder.insert l) v) <*> (builder t o)
    where
      l = SProxy :: SProxy l
      k = reflectSymbol l
      v = getStringValue k o
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
