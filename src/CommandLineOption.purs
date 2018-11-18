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

type CommandLineOptions =  { inFormat :: String, outFormat :: String }

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
    o' = (default "out-format" "json" (default "in-format" "json" o))
    inFormat = unsafePartial (fromJust (Object.lookup "in-format" o'))
    outFormat = unsafePartial (fromJust (Object.lookup "out-format" o'))
  in
    { inFormat, outFormat }
