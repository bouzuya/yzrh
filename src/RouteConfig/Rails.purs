module RouteConfig.Rails
  ( fromString
  ) where

import Bouzuya.HTTP.Method as Method
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import PathTemplate as PathParameter
import PathTemplate as PathTemplate
import Prelude (bind, const, map, pure)
import RouteConfig (Route, RouteConfig)
import YAS (YAS)
import YAS as YAS

routeFromLine :: String -> Maybe Route
routeFromLine line = do
  let p = "^\\s*(delete|get|patch|post|put)\\s+'([^']+)'\\s*,\\s*to:\\s*'([^']+)'.*$"
  r <- either (const Nothing) Just (Regex.regex p noFlags)
  matches <- Regex.match r line
  case (NonEmptyArray.toArray matches) of
    [_, method', path', to'] -> do
      method <- method'
      pathString <- path'
      path <- PathTemplate.fromConfigString pathString
      to <- to'
      pure { method, path, to }
    _ -> Nothing

fromString :: String -> YAS
fromString s =
  let
    lines = String.split (String.Pattern "\n") s
    routes = Array.catMaybes (map routeFromLine lines)
  in
    configToYAS { routes }

configToYAS :: RouteConfig -> YAS
configToYAS config =
  let
    toRoute r = do
      pattern <- YAS.patternFromString "^[^/]+$" -- TODO
      method <- Method.fromString (String.toUpper r.method)
      pure
        { action: r.to
        , method
        , name: r.to
        , parameters:
            map (\name -> { name, pattern }) (PathParameter.parameterNames r.path)
        , path: r.path
        }
    routes = Array.catMaybes (map toRoute config.routes)
  in
    { actions: [] -- TODO
    , routes
    , views: [] -- TODO
    }
