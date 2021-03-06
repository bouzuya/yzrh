module YAS.Json
  ( fromJsonString
  , toJsonString
  ) where

import Prelude

import Bouzuya.HTTP.Method as Method
import Bouzuya.HTTP.StatusCode as StatusCode
import Data.Either as Either
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Traversable as Traversable
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as Object
import PathTemplate as PathTemplate
import Simple.JSON as SimpleJSON
import YAS (Action, Route, YAS, View)
import YAS as YAS

type ActionJson =
  { name :: String
  , parameters :: Array { name :: String }
  , views :: Object { name :: String }
  }

type RouteJson =
  { action :: String
  , method :: String
  , name :: String
  , parameters :: Array { name :: String, pattern :: String }
  , path :: String
  }

type ViewJson =
  { name :: String
  }

type YASJson =
  { actions :: Array ActionJson
  , routes :: Array RouteJson
  , views :: Array ViewJson
  }

fromActionJson :: ActionJson -> Maybe Action
fromActionJson { name, parameters, views } = do
  views' <-
    map
      Map.fromFoldable
      (Traversable.traverse
        (\(Tuple s v) -> do
          i <- Int.fromString s
          statusCode <- StatusCode.fromInt i -- validation
          pure (Tuple i v))
        ((Object.toUnfoldable views) :: Array _))
  pure { name, parameters, views: views' }

fromJsonString :: String -> Maybe YAS
fromJsonString s = join (Either.hush (map fromYASJson (SimpleJSON.readJSON s)))

fromRouteJson :: RouteJson -> Maybe Route
fromRouteJson { action, method, name, parameters, path } = do
  method' <- Method.fromString method
  parameters' <-
    Traversable.traverse
      (\{ name: name', pattern: pattern' } -> do
        pattern'' <- YAS.patternFromString pattern'
        pure { name: name', pattern: pattern'' }
      )
      parameters
  path' <- PathTemplate.fromString path
  pure { action, method: method', name, parameters: parameters', path: path' }

fromViewJson :: ViewJson -> Maybe View
fromViewJson json = pure json

fromYASJson :: YASJson -> Maybe YAS
fromYASJson json = do
  actions <- Traversable.traverse fromActionJson json.actions
  routes <- Traversable.traverse fromRouteJson json.routes
  views <- Traversable.traverse fromViewJson json.views
  pure { actions, routes, views }

toActionJson :: Action -> ActionJson
toActionJson { name, parameters, views } =
  let
    views' =
      Object.fromFoldable
        (map
          (\(Tuple i v) -> Tuple (show i) v)
          ((Map.toUnfoldable views) :: Array (Tuple Int View)))
  in
    { name, parameters, views: views' }

toJson :: YAS -> YASJson
toJson { actions, routes, views } =
  let
    actions' = map toActionJson actions
    routes' = map toRouteJson routes
    views' = map toViewJson views
  in
    { actions: actions', routes: routes', views: views' }

toJsonString :: YAS -> String
toJsonString = SimpleJSON.writeJSON <<< toJson

toRouteJson :: Route -> RouteJson
toRouteJson { action, method, name, parameters, path } =
  let
    method' = show method
    parameters' =
      map
        (\{ name: name', pattern: pattern' } ->
          let pattern'' = show pattern'
          in { name: name', pattern: pattern'' })
        parameters
    path' = show path
  in
    { action, method: method', name, parameters: parameters', path: path' }

toViewJson :: View -> ViewJson
toViewJson = identity
