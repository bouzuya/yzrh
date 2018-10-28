module YAS.RailsRoutes
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
import YAS (YAS)
import YAS as YAS

routeFromLine :: String -> Maybe YAS.Route
routeFromLine line = do
  let p = "^\\s*(delete|get|patch|post|put)\\s+'([^']+)'\\s*,\\s*to:\\s*'([^']+)'.*$"
  r <- either (const Nothing) Just (Regex.regex p noFlags)
  matches <- Regex.match r line
  case (NonEmptyArray.toArray matches) of
    [_, method', path', to'] -> do
      pattern <- YAS.patternFromString "^[^/]+$" -- TODO
      methodString <- method'
      method <- Method.fromString (String.toUpper methodString)
      pathString <- path'
      path <- PathTemplate.fromConfigString pathString
      action <- to'
      pure
        { action
        , method
        , name: action
        , parameters:
            map (\name -> { name, pattern }) (PathParameter.parameterNames path)
        , path
        }
    _ -> Nothing

fromString :: String -> YAS
fromString s =
  let
    lines = String.split (String.Pattern "\n") s
    routes = Array.catMaybes (map routeFromLine lines)
  in
    { actions: [] -- TODO
    , routes
    , views: [] -- TODO
    }
