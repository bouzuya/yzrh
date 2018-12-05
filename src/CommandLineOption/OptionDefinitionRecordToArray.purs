module CommandLineOption.OptionDefinitionRecordToArray
  ( OptionDefinition'
  , booleanOption'
  , maybeStringOption'
  , stringOption'
  ) where

import Data.Maybe (Maybe(..))

type LongName = String
type ShortName = Char
type MetaVar = String
type Help = String
data OptionDefinition' a = OptionDefinition' LongName (Maybe ShortName) (Maybe MetaVar) Help a

booleanOption' :: String -> Maybe Char -> String -> OptionDefinition' Boolean
booleanOption' l s h = OptionDefinition' l s Nothing h true

maybeStringOption' :: String -> Maybe Char -> String -> String -> Maybe String -> OptionDefinition' (Maybe String)
maybeStringOption' l s m h v = OptionDefinition' l s (Just m) h v

stringOption' :: String -> Maybe Char -> String -> String -> String -> OptionDefinition' String
stringOption' l s m h v = OptionDefinition' l s (Just m) h v
