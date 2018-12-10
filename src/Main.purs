module Main
  ( main
  ) where

import CommandLineOption as CommandLineOption
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
import Prelude (class Ord, Unit, bind, discard, map, pure, show, unit, (<>), (==))
import Simple.JSON (writeJSON)
import Unsafe.Coerce (unsafeCoerce)
import YAS (YAS)
import YAS as YAS
import YAS.RailsRoutes as YASRailsRoutes

readRoutesRb :: FilePath -> Effect YAS
readRoutesRb p = map YASRailsRoutes.fromString (FS.readTextFile Encoding.UTF8 p)

readYasJson :: FilePath -> Effect YAS
readYasJson p = unsafeCoerce unit -- FIXME

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
  optionsMaybe <- pure (CommandLineOption.parse (Array.drop 2 argv))
  options <- maybe (throw "no options") pure optionsMaybe
  if options.version
    then do
      log "0.0.0" -- TODO
      Process.exit 0
    else pure unit
  inFile <- maybe (throw "no inFile") pure options.inFile
  yas <-
    if options.inFormat == "json"
    then readYasJson inFile
    else if options.inFormat == "routes.rb"
    then readRoutesRb inFile
    else throw "unknown format"
  if options.outFormat == "json"
    then do
      let
        paths = yasToPaths yas
        info = OpenAPIHelper.buildInfo inFile "0.0.0"
        openApi = OpenAPIHelper.buildOpenApi info paths
      log (writeJSON openApi)
    else if options.outFormat == "routes.rb"
    then throw "not implemented" -- TODO
    else throw "unknown format"
