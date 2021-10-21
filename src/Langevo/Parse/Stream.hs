module Langevo.Parse.Stream
  ( BasicStream (..)
  , Stream (..)
  , CoStream (..)
  , RevStream (..)
  , RevCoStream (..)
  , Cursor
  , cursor
  , cursorPos
  , cursorStream
  , Tape
  , tape
  , preceding
  , following
  ) where

class BasicStream s where
  type Item s
  empty :: s

class BasicStream s => Stream s where
  uncons :: s -> Maybe (Item s, s)

class BasicStream s => CoStream s where
  cons :: Item s -> s -> s

class BasicStream s => RevStream s where
  revUncons :: s -> Maybe (s, Item s)

class BasicStream s => RevCoStream s where
  revCons :: s -> Item s -> s

instance BasicStream [a] where
  type Item [a] = a
  empty = []

instance Stream [a] where
  uncons [] = Nothing
  uncons (x : xs) = Just (x, xs)

instance CoStream [a] where
  cons = (:)

data Tape s = Tape { preceding :: s, following :: s } deriving (Show, Eq)

tape :: BasicStream s => s -> Tape s
tape = Tape empty

instance BasicStream s => BasicStream (Tape s) where
  type Item (Tape s) = Item s
  empty = tape empty

instance (Stream s, CoStream s) => Stream (Tape s) where
  uncons t =
    let next (x, s) = (x, Tape (cons x (preceding t)) s)
    in fmap next (uncons (following t))

instance (Stream s, CoStream s) => CoStream (Tape s) where
  cons x t =
    let 

data Cursor s = Cursor { cursorPos :: Int, cursorStream :: s }
                deriving (Show, Eq)

cursor :: s -> Cursor s
cursor = Cursor 0

instance BasicStream s => BasicStream (Cursor s) where
  type Item (Cursor s) = Item s
  empty = cursor empty

instance Stream s => Stream (Cursor s) where
  uncons c =
    let mapNext (x, s) = (x, Cursor (cursorPos c + 1) s)
    in fmap mapNext (uncons (cursorStream c))

instance CoStream s => CoStream (Cursor s) where
  cons x c = Cursor (cursorPos c - 1) (cons x (cursorStream c))

instance RevStream s => RevStream (Cursor s) where
  revUncons c =
    let mapPrev (s, x) = (Cursor (cursorPos c - 1) s, x)
    in fmap mapPrev (revUncons (cursorStream c))

instance RevCoStream s => RevCoStream (Cursor s) where
  revCons c x = Cursor (cursorPos c + 1) (revCons (cursorStream c) x)
