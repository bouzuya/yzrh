module PathTemplate
  ( PathTemplate
  , fromConfigString
  , params
  ) where

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String as String
import Prelude (class Eq, class Ord, class Show, compare, eq, map, show, (<>))

data T
  = P String
  | S String

instance eqT :: Eq T where
  eq (P s1) (P s2) = eq s1 s2
  eq (S s1) (S s2) = eq s1 s2
  eq _ _ = false

instance showT :: Show T where
  show (P s) = "{" <> s <> "}"
  show (S s) = s

newtype PathTemplate = PathTemplate (Array T)

instance eqPathTemplate :: Eq PathTemplate where
  eq (PathTemplate ts1) (PathTemplate ts2) = eq ts1 ts2

instance ordPathTemplate :: Ord PathTemplate where
  compare p1 p2 = compare (show p1) (show p2)

instance showPathTemplate :: Show PathTemplate where
  show (PathTemplate ts) = intercalate "/" (map show ts)

fromConfigString :: String -> Maybe PathTemplate
fromConfigString s =
  Just (PathTemplate (map toT (String.split (String.Pattern "/") s)))
  where
    toT s' =
      let { before, after } = String.splitAt 1 s'
      in
        case before of
          ":" -> P after
          _ -> S s'

params :: PathTemplate -> Array String
params (PathTemplate ts) =
  Array.catMaybes (map toName ts)
  where
    toName (P s) = Just s
    toName _ = Nothing
