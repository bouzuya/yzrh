module Main
  ( main
  ) where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import OpenAPI as OpenAPI
import OpenAPIHelper as OpenAPIHelper
import PathTemplate (PathTemplate)
import PathTemplate as PathTemplate
import Prelude (class Ord, Unit, bind, map, pure, show, (<>))
import Simple.JSON (writeJSON)
import YAS (YAS)
import YAS as YAS
import YAS.RailsRoutes as YASRailsRoutes

read :: FilePath -> Effect YAS
read p = map YASRailsRoutes.fromString (FS.readTextFile Encoding.UTF8 p)

pathMap :: forall k v. Ord k => (v -> k) -> Array v -> Map k (NonEmptyArray v)
pathMap key xs =
  Map.fromFoldableWith (<>) (map (\x -> Tuple (key x) (NonEmptyArray.singleton x)) xs)

yasToPaths :: YAS -> OpenAPI.Paths
yasToPaths yas =
  let
    entries :: Array (Tuple PathTemplate (NonEmptyArray YAS.Route))
    entries = Map.toUnfoldable (pathMap _.path yas.routes)
    tuple :: Tuple PathTemplate (NonEmptyArray YAS.Route) -> Tuple String OpenAPI.PathItem
    tuple (Tuple path routes) =
      let
        operationMap =
          Map.fromFoldable
            ( map
                (\r ->
                  Tuple
                    (String.toLower (show r.method))
                    (OpenAPIHelper.buildOperation (PathTemplate.parameterNames r.path))
                )
                routes
            )
        pathItem =
          OpenAPIHelper.buildPathItem
            (show path)
            operationMap
      in
        Tuple (show path) pathItem
    paths :: OpenAPI.Paths
    paths = Object.fromFoldable (map tuple entries)
  in
    paths

main :: Effect Unit
main = do
  argv <- Process.argv
  file <- maybe (throw "no arg") pure (argv Array.!! 1)
  config <- read file
  let
    paths = yasToPaths config
    info = OpenAPIHelper.buildInfo file "0.0.0"
    openApi = OpenAPIHelper.buildOpenApi info paths
  log (writeJSON openApi)
