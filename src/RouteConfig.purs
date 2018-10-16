module RouteConfig
  ( Route
  , RouteConfig
  , fromString
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Prelude (bind, const, map, pure)

type Route =
  { method :: String
  , path :: String
  , to :: String
  }

type RouteConfig =
  { routes :: Array Route
  }

routeFromLine :: String -> Maybe Route
routeFromLine line = do
  let p = "^\\s*(delete|get|patch|post|put)\\s+'([^']+)',\\s*to:\\s*'([^']+)'.*$"
  r <- either (const Nothing) Just (Regex.regex p noFlags)
  matches <- Regex.match r line
  case (NonEmptyArray.toArray matches) of
    [_, method', path', to'] -> do
      method <- method'
      path <- path'
      to <- to'
      pure { method, path, to }
    _ -> Nothing

fromString :: String -> RouteConfig
fromString s =
  let
    lines = String.split (String.Pattern "\n") s
    routes = Array.catMaybes (map routeFromLine lines)
  in
    { routes }
