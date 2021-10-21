module Langevo.Parse.Error where

import Prelude hiding (Either, not)

data Expected a
  = Item a
  | Any
  | Not (Expected a)
  | Either [Expected a]
  | Sequence [Expected a]
  deriving (Show, Eq)

either :: Expected a -> Expected a -> Expected a
either (Either xs) (Either ys) = Either (xs ++ ys)
either e (Either xs) = Either (e : xs)
either (Either xs) e = Either (xs ++ [e])
either e1 e2 = Either [e1, e2]

sequence :: Expected a -> Expected a -> Expected a
sequence (Sequence xs) (Sequence ys) = Sequence (xs ++ ys)
sequence e (Sequence xs) = Sequence (e : xs)
sequence (Sequence xs) e = Sequence (xs ++ [e])
sequence e1 e2 = Sequence [e1, e2]

not :: Expected a -> Expected a
not (Not (Not e)) = not e
not (Not e) = e
not e = Not e

data Error s a = Error { stream :: s, expected :: Expected a } deriving (Show)
