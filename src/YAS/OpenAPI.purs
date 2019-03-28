module YAS.OpenAPI
  ( fromYAS
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.String as String
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import OpenAPI (OpenAPI, Paths, PathItem)
import OpenAPIHelper as OpenAPIHelper
import PathTemplate (PathTemplate)
import PathTemplate as PathTemplate
import YAS (YAS, Route)

fromYAS :: YAS -> String -> String -> OpenAPI
fromYAS yas title version =
  let
    paths = yasToPaths yas
    info = OpenAPIHelper.buildInfo title version
    openApi = OpenAPIHelper.buildOpenApi info paths
  in
    openApi

pathMap :: forall k v. Ord k => (v -> k) -> Array v -> Map k (NonEmptyArray v)
pathMap key xs =
  Map.fromFoldableWith
    (<>)
    (map (\x -> Tuple (key x) (NonEmptyArray.singleton x)) xs)

yasToPaths :: YAS -> Paths
yasToPaths yas =
  let
    entries :: Array (Tuple PathTemplate (NonEmptyArray Route))
    entries = Map.toUnfoldable (pathMap _.path yas.routes)
    tuple :: Tuple PathTemplate (NonEmptyArray Route) -> Tuple String PathItem
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
    paths :: Paths
    paths = Object.fromFoldable (map tuple entries)
  in
    paths
