module CommandLineOption
  ( parse
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, pure, (<<<))

type CommandLineOptions =  { inFile :: String, inFormat :: String, outFormat :: String }

default :: String -> String -> Object String -> Object String
default k v o = Object.alter (maybe (Just v) Just) k o

parse :: Array String -> Maybe CommandLineOptions
parse = toRecord <<< toObject

-- "--name" -> "name"
toName :: String -> String
toName = String.drop 2

-- ["--name", "value"] -> { name: "value" }
toObject :: Array String -> Object String
toObject options = f options Object.empty
  where
    f o p =
      case Array.take 2 o of
        [] -> p
        [_] -> p
        [key, value] -> f (Array.drop 2 o) (Object.insert (toName key) value p)
        _ -> p

toRecord :: Object String -> Maybe CommandLineOptions
toRecord o = do
  let o' = (default "out-format" "json" (default "in-format" "json" o))
  inFile <- Object.lookup "in-file" o'
  inFormat <- Object.lookup "in-format" o'
  outFormat <- Object.lookup "out-format" o'
  pure { inFile, inFormat, outFormat }
