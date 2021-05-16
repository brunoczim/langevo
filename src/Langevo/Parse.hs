module Langevo.Parse
  (
  ) where

import Data.Text (Text)
import Text.Megaparsec
  ( Parsec
  , MonadParsec (eof, lookAhead, try)
  , parseError
  , (<|>)
  , customFailure
  , choice
  , chunk
  , anySingle
  , manyTill
  , runParser
  , ParseErrorBundle
  )
import Text.Megaparsec.Char (space, space1)
import Prelude hiding (Word)
import Data.Void (Void)

type Symbol = Text
type Tape = [Symbol]
type ErrorData = Void
type TextParser = Parsec ErrorData Text
type TapeParser = Parsec ErrorData Tape

data SymbolForm = SymbolForm
  { symbolMain :: Symbol
  , symbolVariants :: [Symbol]
  } deriving (Eq, Ord, Read, Show)

forms :: Symbol -> [Symbol] -> SymbolForm
forms main vars = SymbolForm main (main : vars)

parseSymbol :: [SymbolForm] -> TextParser Symbol
parseSymbol = 
  let parseForm :: SymbolForm -> TextParser Symbol
      parseForm form = 
        let choices = fmap chunk (symbolVariants form)
        in fmap (const (symbolMain form)) (choice choices)
  in choice . fmap parseForm

parseTape :: [SymbolForm] -> TextParser Tape
parseTape forms = manyTill (parseSymbol forms) eof
  
translateTape :: TapeParser Tape -> TapeParser Tape
translateTape parser =
  let parseCurr = do
        head <- try parser <|> fmap (: []) anySingle
        tail <- translateTape parser
        return (head ++ tail)
  in fmap (const []) eof <|> parseCurr
