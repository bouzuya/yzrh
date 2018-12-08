module Bouzuya.CommandLineOption
  ( parse
  ) where

import Bouzuya.CommandLineOption.ObjectToRecord (class OptionRecordBuilder)
import Bouzuya.CommandLineOption.ObjectToRecord as ObjectToRecord
import Bouzuya.CommandLineOption.OptionDefinition (NamedOptionDefinition)
import Bouzuya.CommandLineOption.OptionObject as OptionObject
import Bouzuya.CommandLineOption.RecordToArray (class ArrayBuilder)
import Bouzuya.CommandLineOption.RecordToArray as RecordToArray
import Data.Either (Either, note)
import Prelude (bind, pure)
import Prim.RowList (class RowToList)

type Parsed r =
  { arguments :: Array String
  , options :: Record r
  }

parse :: forall r1 r2 l1 l2
   . RowToList r1 l1
  => ArrayBuilder l1 () r1 NamedOptionDefinition
  => RowToList r2 l2
  => OptionRecordBuilder l2 () r2
  => Record r1
  -> Array String
  -> Either String (Parsed r2)
parse defs ss = do
  ds <- pure (RecordToArray.toArray defs)
  parsed <-  OptionObject.parse ds ss
  options <- note "toRecord error" (ObjectToRecord.toRecord parsed.options)
  pure { arguments: parsed.arguments, options }
