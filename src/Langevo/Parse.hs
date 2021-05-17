module Langevo.Parse
  ( mainSym
  , symVariants
  , symForm
  , parseTape
  , translateTape
  , TextParser
  , Symbol
  , TapeParser
  , Tape
  , ErrorData
  , Error
  , shiftTape
  ) where

import Data.Text (Text)
import Text.Megaparsec
  ( Parsec
  , MonadParsec (eof, try)
  , (<|>)
  , choice
  , chunk
  , anySingle
  , manyTill
  , ParseErrorBundle
  , runParser
  )
import Data.Void (Void)
import Data.Foldable (foldl')
import Data.Either (fromRight)

type Symbol = Text
type Tape = [Symbol]
type ErrorData = Void
type Error s = ParseErrorBundle s ErrorData
type TextParser = Parsec ErrorData Text
type TapeParser = Parsec ErrorData Tape

mainSym :: Symbol -> TextParser a -> TextParser Tape
mainSym res = fmap (const [res])

symVariants :: Symbol -> [Symbol] -> TextParser Tape
symVariants main alts = mainSym main (choice (fmap chunk alts))

symForm :: Symbol -> [Symbol] -> TextParser Tape
symForm main alts = symVariants main (main : alts)

parseTape :: [TextParser Tape] -> TextParser Tape
parseTape parsers = fmap mconcat (manyTill (choice parsers) eof)
  
translateTape :: TapeParser Tape -> Tape -> Tape
translateTape parser = fromRight undefined . runParser translator "tape-trans" 
  where
    translator = 
      let parseCurr = do
            head <- try parser <|> fmap (: []) anySingle
            tail <- translator
            return (head ++ tail)
      in fmap (const []) eof <|> parseCurr

shiftTape :: [TapeParser Tape] -> Tape -> [Tape]
shiftTape shifts initial = last : forms
  where
    folder (last, forms) shift =
      let last' = translateTape shift last
      in if last' == last then (last, forms) else (last', last : forms)
    (last, forms) = foldl' folder (initial, []) shifts
