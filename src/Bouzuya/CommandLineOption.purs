module Bouzuya.CommandLineOption
  ( class DefsToVals
  , parse
  ) where

import Bouzuya.CommandLineOption.ObjectToRecord (class OptionRecordBuilder)
import Bouzuya.CommandLineOption.ObjectToRecord as ObjectToRecord
import Bouzuya.CommandLineOption.OptionDefinition (NamedOptionDefinition, TypedOptionDefinition)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Bouzuya.CommandLineOption.RecordToArray (class ArrayBuilder)
import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Data.Either (Either, note)
import Prelude (bind, pure)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Type.Row (class ListToRow)

type Parsed r =
  { arguments :: Array String
  , options :: Record r
  }

class DefsToVals (l1 :: RowList) (l2 :: RowList) | l1 -> l2
instance defsToValsCons :: DefsToVals t1 t2 => DefsToVals (Cons l (TypedOptionDefinition a) t1) (Cons l a t2)
instance defsToValsNil :: DefsToVals Nil Nil

parse :: forall r1 r2 l1 l2 l3
   . RowToList r1 l1
  => ArrayBuilder l1 () r1 NamedOptionDefinition
  => RowToList r2 l2
  => OptionRecordBuilder l2 () r2
  => DefsToVals l1 l3
  => ListToRow l3 r2
  => Record r1
  -> Array String
  -> Either String (Parsed r2)
parse defs ss = do
  ds <- pure (RecordToArray.toArray defs)
  parsed <-  OptionObject.parse ds ss
  options <- note "toRecord error" (ObjectToRecord.toRecord parsed.options)
  pure { arguments: parsed.arguments, options }
