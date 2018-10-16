module Main
  ( main
  ) where

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding as Encoding
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Process as Process
import Prelude (class Ord, Unit, bind, map, pure, show, (<>))
import RouteConfig as RouteConfig

pathMap :: forall k v. Ord k => (v -> k) -> Array v -> Map k (NonEmptyArray v)
pathMap key xs =
  Map.fromFoldableWith (<>) (map (\x -> Tuple (key x) (NonEmptyArray.singleton x)) xs)

read :: FilePath -> Effect RouteConfig.RouteConfig
read p = map RouteConfig.fromString (FS.readTextFile Encoding.UTF8 p)

main :: Effect Unit
main = do
  argv <- Process.argv
  file <- maybe (throw "no arg") pure (argv Array.!! 1)
  config <- read file
  let m = pathMap _.path config.routes
  log (show m)
