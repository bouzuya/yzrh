module RouteConfig
  ( Route
  , RouteConfig
  ) where

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import PathTemplate (PathTemplate)
import PathTemplate as PathTemplate
import Prelude (bind, const, map, pure)

type Route =
  { method :: String
  , path :: PathTemplate
  , to :: String
  }

type RouteConfig =
  { routes :: Array Route
  }
