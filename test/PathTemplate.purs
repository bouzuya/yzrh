module Test.PathTemplate
  ( tests
  ) where

import Prelude

import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import PathTemplate as PathTemplate
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "PathTemplate" do
  test "/users/:id" do
    let
      template =
        unsafePartial (fromJust (PathTemplate.fromConfigString "/users/:id"))
    Assert.equal "/users/{id}" (show template)
    Assert.equal ["id"] (PathTemplate.params template)
  test "/" do
    let
      template =
        unsafePartial (fromJust (PathTemplate.fromConfigString "/"))
    Assert.equal "/" (show template)
    Assert.equal [] (PathTemplate.params template)
  test "Eq" do
    Assert.assert
      "=="
      ((PathTemplate.fromConfigString "/users/:id") == (PathTemplate.fromConfigString "/users/:id"))
    Assert.assert
      "/="
      ((PathTemplate.fromConfigString "/") /= (PathTemplate.fromConfigString "/users/:id"))
