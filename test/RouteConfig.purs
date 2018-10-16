module Test.RouteConfig
  ( tests
  ) where

import RouteConfig as RouteConfig
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "RouteConfig" do
  test "basic" do
    Assert.equal
      { routes:
        [ { method: "post", path: "/users", to: "users#create" }
        , { method: "get", path: "/users/:id", to: "users#show" }
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
