module Main
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception as Exception
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import Options as Options
import Simple.JSON as SimpleJSON
import Version as Version
import YAS (YAS)
import YAS.Json as YASJson
import YAS.OpenAPI as YASOpenAPI
import YAS.RailsRoutes as YASRailsRoutes

read :: String -> String -> Effect YAS
read format file =
  case format of
    "json" -> readYasJson file
    "routes.rb" -> readRoutesRb file
    _ -> Exception.throw "unknown format"

readRoutesRb :: FilePath -> Effect YAS
readRoutesRb p = map YASRailsRoutes.fromString (FS.readTextFile Encoding.UTF8 p)

readYasJson :: FilePath -> Effect YAS
readYasJson p = do
  jsonString <- FS.readTextFile Encoding.UTF8 p
  Maybe.maybe
    (Exception.throw "invalid yas.json")
    pure
    (YASJson.fromJsonString jsonString)

write :: String -> YAS -> String -> String -> Effect Unit
write format yas title version =
  case format of
    "json" -> writeYasJson yas
    "openapi.json" -> writeOpenapiJson yas title version
    _ -> Exception.throw "unknown format"

writeOpenapiJson :: YAS -> String -> String -> Effect Unit
writeOpenapiJson yas title version = do
  let openApi = YASOpenAPI.fromYAS yas title version
  Console.log (SimpleJSON.writeJSON openApi)

writeYasJson :: YAS -> Effect Unit
writeYasJson _ = Exception.throw "not implemented" -- FIXME

main :: Effect Unit
main = do
  argv <- Process.argv
  optionsMaybe <- pure (Options.parse (Array.drop 2 argv))
  options <- Maybe.maybe (Exception.throw "no options") pure optionsMaybe
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
  inFile <- Maybe.maybe (Exception.throw "no inFile") pure options.inFile
  main' options.inFormat inFile options.outFormat

main' :: String -> String -> String -> Effect Unit
main' inFormat inFile outFormat = do
  yas <- read inFormat inFile
  let
    title = inFile
    version = "0.0.0"
  write outFormat yas title version
