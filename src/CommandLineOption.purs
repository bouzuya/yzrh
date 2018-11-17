module CommandLineOption
  ( parse
  ) where

import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Prelude ((<<<))

type CommandLineOptions =  { from :: String, to :: String }

default :: String -> String -> Object String -> Object String
default k v o = Object.alter (maybe (Just v) Just) k o

parse :: Array String -> CommandLineOptions
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

toRecord :: Object String -> CommandLineOptions
toRecord o =
  let
    o' = (default "to" "json" (default "from" "json" o))
    from = unsafePartial (fromJust (Object.lookup "from" o'))
    to = unsafePartial (fromJust (Object.lookup "to" o'))
  in
    { from, to }
