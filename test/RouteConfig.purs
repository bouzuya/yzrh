module Test.RouteConfig
  ( tests
  ) where

import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import PathTemplate as PathTemplate
import RouteConfig as RouteConfig
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "RouteConfig" do
  test "basic" do
    let p s = unsafePartial (fromJust (PathTemplate.fromConfigString s))
    Assert.equal
      { routes:
        [ { method: "post", path: p "/users", to: "users#create" }
        , { method: "get", path: p "/users/:id", to: "users#show" }
        ]
      }
      (
        RouteConfig.fromString """
          Rails.application.routes.draw do
            post '/users', to: 'users#create'
            get '/users/:id', to: 'users#show'
          end
        """
      )
