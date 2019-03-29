module YAS
  ( Action
  , ActionName
  , ActionParameter
  , Pattern
  , PathParameter
  , Route
  , View
  , ViewName
  , YAS
  , patternFromString
  ) where

import Prelude

import Bouzuya.HTTP.Method (Method)
import Data.Either as Either
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import PathTemplate (PathTemplate)

type Action =
  { name :: ActionName
  , parameters :: Array ActionParameter
  , views :: Map Int View -- Int (StatusCode)
  }

type ActionName = String

type ActionParameter =
  { name :: String
  }

newtype Pattern = Pattern Regex

instance eqPattern :: Eq Pattern where
  eq (Pattern r1) (Pattern r2) = eq (show r1) (show r2)

instance showPattern :: Show Pattern where
  show (Pattern r) = show r

type PathParameter =
  { name :: String
  , pattern :: Pattern
  }

type Route =
  { action :: ActionName
  , method :: Method
  , name :: String
  , parameters :: Array PathParameter
  , path :: PathTemplate
  }

type View =
  { name :: ViewName
  }

type ViewName = String

type YAS =
  { actions :: Array Action
  , routes :: Array Route
  , views :: Array View
  }

patternFromString :: String -> Maybe Pattern
patternFromString s =
  map Pattern (Either.hush (Regex.regex s noFlags))
