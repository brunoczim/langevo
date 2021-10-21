module Langevo.Parse
  ( Parser
  ) where

import Langevo.Parse.Stream (Stream, Item, uncons)
import Langevo.Parse.Error (Error (Error))
import qualified Langevo.Parse.Error as Error
import Langevo.Parse.Generic (runParser, parser)
import qualified Langevo.Parse.Generic as Generic

type Parser s t a = Generic.Parser s (Error s t) a

raise :: Error.Expected t -> Parser s t a
raise err = parser $ \s -> Left (Error s err)

next :: Stream s => Parser s (Item s) (Item s)
next = parser $ \s -> case uncons s of
  Just (x, s') -> pure (x, s')
  Nothing -> runParser (raise Error.Any) s
