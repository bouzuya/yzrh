module CommandLineOption.OptionValue
  ( OptionValue
  , fromBoolean
  , fromString
  , getBooleanValue
  , getStringValue
  ) where

import Data.Maybe (Maybe(..))

data OptionValue
  = BooleanValue Boolean
  | StringValue String

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
