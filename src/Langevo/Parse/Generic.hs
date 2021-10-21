module Langevo.Parse.Generic
  ( Parser
  , runParser
  , parser
  ) where

newtype Parser s e a = Parser { runParser :: s -> Either e (a, s) }

parser :: (s -> Either e (a, s)) -> Parser s e a
parser = Parser

instance Functor (Parser s e) where
  fmap f p = parser $ \s -> fmap (\(x, s') -> (f x, s')) (runParser p s)

instance Applicative (Parser s e) where
  pure x = parser $ \s -> pure (x, s)
  p <*> q = parser $ \s -> do
    (f, s') <- runParser p s
    (x, s'') <- runParser q s'
    return (f x, s'')

instance Monad (Parser s e) where
  p >>= f = parser $ \s -> do
    (x, s') <- runParser p s
    runParser (f x) s'

fail :: e -> Parser s e a
fail = parser . const . Left

instance Semigroup (Parser s e a) where
  p <> q = parser $ \s -> runParser p s <> runParser q s

