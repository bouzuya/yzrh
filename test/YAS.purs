module Test.RouteConfig
  ( tests
  ) where

import Bouzuya.HTTP.Method as Method
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import PathTemplate as PathTemplate
import YAS.RailsRoutes as YASRailsRoutes
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import YAS as YAS

tests :: TestSuite
tests = suite "YAS.RailsRoutes" do
  test "basic" do
    let
      p s = unsafePartial (fromJust (PathTemplate.fromConfigString s))
      pattern = unsafePartial (fromJust (YAS.patternFromString "^[^/]+$"))
    Assert.equal
      { actions: []
      , routes:
        [ { action: "users#create", method: Method.POST, name: "users#create", parameters: [], path: p "/users" }
        , { action: "users#show", method: Method.GET, name: "users#show", parameters: [{ name: "id", pattern }], path: p "/users/:id" }
        ]
      , views: []
      }
      (
        YASRailsRoutes.fromString """
          Rails.application.routes.draw do
            post '/users', to: 'users#create'
            get '/users/:id', to: 'users#show'
          end
        """
      )
