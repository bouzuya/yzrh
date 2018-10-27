module Main
  ( main
  ) where

import Bouzuya.HTTP.Method as Method
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
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
import PathTemplate as PathParameter
import PathTemplate as PathTemplate
import Prelude (class Ord, Unit, bind, const, map, pure, show, (<<<), (<>))
import RouteConfig (Route, RouteConfig)
import RouteConfig as RouteConfig
import RouteConfig.Rails as RouteConfigRails
import Simple.JSON (writeJSON)
import YAS (YAS)
import YAS as YAS

pathMap :: forall k v. Ord k => (v -> k) -> Array v -> Map k (NonEmptyArray v)
pathMap key xs =
  Map.fromFoldableWith (<>) (map (\x -> Tuple (key x) (NonEmptyArray.singleton x)) xs)

read :: FilePath -> Effect RouteConfig.RouteConfig
read p = map RouteConfigRails.fromString (FS.readTextFile Encoding.UTF8 p)

configToYAS :: RouteConfig -> YAS
configToYAS config =
  let
    toRoute r = do
      pattern <- either (const Nothing) Just (Regex.regex "^[^/]+$" noFlags) -- TODO
      method <- Method.fromString r.method
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
                    (show r.method)
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

configToPaths' :: RouteConfig -> OpenAPI.Paths
configToPaths' = yasToPaths <<< configToYAS

configToPaths :: RouteConfig -> OpenAPI.Paths
configToPaths config =
  let
    m = pathMap _.path config.routes
    entries :: Array (Tuple PathTemplate (NonEmptyArray Route))
    entries = Map.toUnfoldable m
    tuple :: Tuple PathTemplate (NonEmptyArray Route) -> Tuple String OpenAPI.PathItem
    tuple (Tuple path routes) =
      let
        operationMap =
          Map.fromFoldable
            ( map
                (\r ->
                  Tuple
                    r.method
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
    paths = configToPaths config
    info = OpenAPIHelper.buildInfo file "0.0.0"
    openApi = OpenAPIHelper.buildOpenApi info paths
  log (writeJSON openApi)
