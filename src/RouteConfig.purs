module RouteConfig
  ( Route
  , RouteConfig
  ) where

import PathTemplate (PathTemplate)

type Route =
  { method :: String
  , path :: PathTemplate
  , to :: String
  }

type RouteConfig =
  { routes :: Array Route
  }
