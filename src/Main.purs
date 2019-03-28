module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (throw)
import Effect.Exception as Exception
import Foreign.Object as Object
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import OpenAPI as OpenAPI
import OpenAPIHelper as OpenAPIHelper
import Options as Options
import PathTemplate (PathTemplate)
import PathTemplate as PathTemplate
import Simple.JSON (writeJSON)
import Version as Version
import YAS (YAS)
import YAS as YAS
import YAS.Json as YASJson
import YAS.RailsRoutes as YASRailsRoutes

readRoutesRb :: FilePath -> Effect YAS
readRoutesRb p = map YASRailsRoutes.fromString (FS.readTextFile Encoding.UTF8 p)

readYasJson :: FilePath -> Effect YAS
readYasJson p = do
  jsonString <- FS.readTextFile Encoding.UTF8 p
  Maybe.maybe
    (Exception.throw "invalid yas.json")
    pure
    (YASJson.fromJsonString jsonString)

writeOpenapiJson :: YAS -> String -> Effect Unit
writeOpenapiJson yas title = do
  let
    paths = yasToPaths yas
    info = OpenAPIHelper.buildInfo title "0.0.0"
    openApi = OpenAPIHelper.buildOpenApi info paths
  Console.log (writeJSON openApi)

writeYasJson :: YAS -> Effect Unit
writeYasJson _ = Exception.throw "not implemented" -- FIXME

pathMap :: forall k v. Ord k => (v -> k) -> Array v -> Map k (NonEmptyArray v)
pathMap key xs =
  Map.fromFoldableWith
    (<>)
    (map (\x -> Tuple (key x) (NonEmptyArray.singleton x)) xs)

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
                    (OpenAPIHelper.buildOperation
                      (PathTemplate.parameterNames r.path))
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
  optionsMaybe <- pure (Options.parse (Array.drop 2 argv))
  options <- maybe (throw "no options") pure optionsMaybe
  if options.help
    then do
      Console.log Options.help
      Process.exit 0
    else pure unit
  if options.version
    then do
      Console.log Version.version
      Process.exit 0
    else pure unit
  inFile <- maybe (throw "no inFile") pure options.inFile
  yas <-
    case options.inFormat of
      "json" -> readYasJson inFile
      "routes.rb" -> readRoutesRb inFile
      _ -> Exception.throw "unknown format"
  case options.outFormat of
    "json" -> writeYasJson yas
    "openapi.json" -> writeOpenapiJson yas inFile -- inFile as title
    _ -> Exception.throw "unknown format"
