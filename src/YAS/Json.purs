module YAS.Json
  ( fromJsonString ) where

import Data.Either (either)
import Data.Maybe (Maybe(..))
import Prelude (const, (<<<))
import Simple.JSON (readJSON)
import YAS (YAS)

type YASJson =
  { -- TODO
  }

fromJsonString :: String -> Maybe YAS
fromJsonString s =
  either (const Nothing) (Just <<< toYAS) (readJSON s)

-- TODO
toYAS :: YASJson -> YAS
toYAS _ =
  { actions: []
  , routes: []
  , views: []
  }
