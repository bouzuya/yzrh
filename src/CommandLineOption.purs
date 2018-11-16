module CommandLineOption
  ( parse
  ) where

import Data.Array as Array

parse :: Array String -> { to :: String }
parse xs = case Array.take 2 xs of
  [] -> { to: "json" }
  [_] -> { to: "json" }
  ["--to", to] -> { to }
  _ -> parse (Array.drop 1 xs)
