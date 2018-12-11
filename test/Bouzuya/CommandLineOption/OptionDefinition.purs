module Test.Bouzuya.CommandLineOption.OptionDefinition
  ( tests
  ) where

import Bouzuya.CommandLineOption (TypedOptionDefinition, booleanOption, maybeStringOption, stringOption)
import Bouzuya.CommandLineOption.NamedOptionDefinition (NamedOptionDefinition, getName, withName)
import Bouzuya.CommandLineOption.TypedOptionDefinition (fromTyped)
import Bouzuya.CommandLineOption.UntypedOptionDefinition (UntypedOptionDefinition, getDefaultValue, getLongName, getShortName, isValueMultiple, isValueRequired)
import Data.Maybe (Maybe(..))
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.CommandLineOption.OptionDefinition" do
  let
    -- public apis
    s :: TypedOptionDefinition String
    s = stringOption "st" (Just 's') "ST" "st help" "default"
    b :: TypedOptionDefinition Boolean
    b = booleanOption "bo" (Just 'b') "bo help"
    m :: TypedOptionDefinition (Maybe String)
    m = maybeStringOption "ms" (Just 'm') "MS" "ms help" Nothing
    defs = { str: s, boo: b, may: m }
    -- private apis
    s' :: UntypedOptionDefinition
    s' = fromTyped s
    b' :: UntypedOptionDefinition
    b' = fromTyped b
    m' :: UntypedOptionDefinition
    m' = fromTyped m
    s'' :: NamedOptionDefinition
    s'' = withName "str" s'
    b'' :: NamedOptionDefinition
    b'' = withName "boo" b'
    m'' :: NamedOptionDefinition
    m'' = withName "may" m'
    defs' = [ s', b', m' ]
  test "getDefaultValue" do
    Assert.equal (Just ["default"]) (getDefaultValue s')
    Assert.equal Nothing (getDefaultValue b')
    Assert.equal Nothing (getDefaultValue m')
  test "getLongName" do
    Assert.equal "st" (getLongName s')
    Assert.equal "bo" (getLongName b')
    Assert.equal "ms" (getLongName m')
  test "getName" do
    Assert.equal "str" (getName s'')
    Assert.equal "boo" (getName b'')
    Assert.equal "may" (getName m'')
  test "getShortName" do
    Assert.equal (Just 's') (getShortName s')
    Assert.equal (Just 'b') (getShortName b')
    Assert.equal (Just 'm') (getShortName m')
  test "isValueRequired" do
    Assert.equal true (isValueRequired s')
    Assert.equal false (isValueRequired b')
    Assert.equal true (isValueRequired m')
  test "isValueMultiple" do
    Assert.equal false (isValueMultiple s')
    Assert.equal false (isValueMultiple b')
    Assert.equal false (isValueMultiple m')
