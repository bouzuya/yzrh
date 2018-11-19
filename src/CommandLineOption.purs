module CommandLineOption
  ( parse
  ) where

import Data.Array (foldl)
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Foreign.Object (Object)
import Foreign.Object as Object
import Prelude (bind, eq, map, not, pure, (&&), (<<<), (==))

type StringOption =
  { help :: String
  , long :: String
  , metavar :: String
  , value :: Maybe String
  }

type CommandLineOptions =  { inFile :: String, inFormat :: String, outFormat :: String }

optionDefinitions :: Array StringOption
optionDefinitions =
  [ { help: "input file"
    , long: "in-file"
    , metavar: "<file>"
    , value: Nothing
    }
  , { help: "input file format"
    , long: "in-format"
    , metavar: "<format>"
    , value: Just "json"
    }
  , { help: "output file format"
    , long: "out-format"
    , metavar: "<format>"
    , value: Just "json"
    }
  ]

default :: String -> String -> Object String -> Object String
default k v o = Object.alter (maybe (Just v) Just) k o

parse :: Array String -> Maybe CommandLineOptions
parse = toRecord <<< toObject

-- "--name" -> "name"
toName :: String -> String
toName = String.drop 2

-- ["--name", "value"] -> { name: "value" }
toObject :: Array String -> Object String
toObject options = setDefaults (toObject' options Object.empty)
  where
    getOptionName s =
      case String.take 2 s of
        "--" -> Just (String.drop 2 s)
        _ -> Nothing
    isOption s = String.take 1 s == "-"
    toObject' o p =
      case Array.head o of
        Nothing -> p
        Just s ->
          case getOptionName s of
            Nothing -> p -- ERROR: support `--option value` format only
            Just long ->
              -- TODO: long == "" -- double hyphen (--) support
              case Array.find (\op -> op.long == long) optionDefinitions of
                Nothing -> p -- ERROR: no such option
                Just d ->
                  case Array.head (Array.drop 1 o) of -- read metavar
                    Nothing -> p -- ERROR: no metavar (end)
                    Just value -> -- TODO: value == "--foo" -- no metavar (next option)
                      toObject' (Array.drop 2 o) (Object.insert d.long value p)
    setDefaults object =
      foldl
        (\o d ->
          case d.value of
            Nothing -> o
            Just v -> default d.long v o)
        object
        optionDefinitions

toRecord :: Object String -> Maybe CommandLineOptions
toRecord o' = do
  inFile <- Object.lookup "in-file" o'
  inFormat <- Object.lookup "in-format" o'
  outFormat <- Object.lookup "out-format" o'
  pure { inFile, inFormat, outFormat }
