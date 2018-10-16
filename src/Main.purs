module Main
  ( main
  ) where

import Data.Array as Array
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import Prelude (Unit, bind, map, pure, show)
import RouteConfig as RouteConfig

read :: FilePath -> Effect RouteConfig.RouteConfig
read p = map RouteConfig.fromString (FS.readTextFile Encoding.UTF8 p)

main :: Effect Unit
main = do
  argv <- Process.argv
  file <- maybe (throw "no arg") pure (argv Array.!! 1)
  config <- read file
  log (show config)
