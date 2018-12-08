module Bouzuya.CommandLineOption.OptionValue
  ( OptionValue
  , fromBoolean
  , fromString
  , getBooleanValue
  , getStringValue
  ) where

import Data.Maybe (Maybe(..))
import Prelude (class Eq, class Show, eq, show)

data OptionValue
  = BooleanValue Boolean
  | StringValue String

instance eqOptionValue :: Eq OptionValue where
  eq (BooleanValue b1) (BooleanValue b2) = eq b1 b2
  eq (StringValue s1) (StringValue s2) = eq s1 s2
  eq _ _ = false

instance showOptionValue :: Show OptionValue where
  show (BooleanValue b) = show b
  show (StringValue s) = show s

fromBoolean :: Boolean -> OptionValue
fromBoolean = BooleanValue

fromString :: String -> OptionValue
fromString = StringValue

getBooleanValue :: OptionValue -> Maybe Boolean
getBooleanValue (BooleanValue b) = Just b
getBooleanValue (StringValue _) = Nothing

getStringValue :: OptionValue -> Maybe String
getStringValue (BooleanValue _) = Nothing
getStringValue (StringValue s) = Just s
