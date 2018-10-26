module YAS
  ( Action
  , ActionName
  , ActionParameter
  , PathParameter
  , Route
  , View
  , ViewName
  , YAS
  ) where

import Bouzuya.HTTP.Method (Method)
import Bouzuya.HTTP.StatusCode (StatusCode)
import Data.Map (Map)
import Data.String.Regex (Regex)
import PathTemplate (PathTemplate)

type Action =
  { name :: ActionName
  , params :: Array ActionParameter
  , view :: Map StatusCode View
  }

type ActionName = String

type ActionParameter =
  { name :: String
  }

type PathParameter =
  { name :: String
  , pattern :: Regex
  }

type Route =
  { action :: ActionName
  , method :: Method
  , name :: String
  , params :: Array PathParameter
  , path :: PathTemplate
  }

type View =
  { name :: ViewName
  }

type ViewName = String

type YAS =
  { route :: Array Route
  , action :: Array Action
  , view :: Array View
  }