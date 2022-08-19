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
  , DoubleTape
  , DoubleTapeParser
  , doubleTape
  , tapeBefore
  , tapeAfter
  , shiftTape
  , wordInitial
  , wordInitially
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
  , getParserState
  , stateInput
  , failure
  , Stream
    ( Token
    , Tokens
    , tokensToChunk
    , chunkToTokens
    , chunkLength
    , take1_
    , takeN_
    , takeWhile_
    )
  )
import qualified Text.Megaparsec.Error as ME
import Data.Void (Void)
import Data.Foldable (foldl')
import Data.Either (fromRight)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty ((:|)))

data DoubleTape
  = DoubleTape { tapeBefore :: Tape, tapeAfter :: Tape }
  deriving (Show, Eq)

doubleTape :: Tape -> DoubleTape
doubleTape = DoubleTape []

instance Stream DoubleTape where
  type Token DoubleTape = Symbol
  type Tokens DoubleTape = Tape

  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  take1_ t = case tapeAfter t of
    [] -> Nothing
    (x : xs) -> Just (x, DoubleTape (x : tapeBefore t) xs)
  takeN_ n t
    | n <= 0 = Just ([], t)
    | n > 0 =
      let makeRet x (xs, t) = (x : xs, t)
          makeNext (x, t) = fmap (makeRet x) (takeN_ (n - 1) t)
      in take1_ t >>= makeNext
  takeWhile_ p t = case take1_ t of
    Just (x, t') | p x ->
      let (xs, t'') = takeWhile_ p t''
      in (x : xs, t'')
    _ -> ([], t)

type Symbol = Text
type Tape = [Symbol]
type ErrorData = Void
type Error s = ParseErrorBundle s ErrorData
type TextParser = Parsec ErrorData Text
type TapeParser = Parsec ErrorData Tape
type DoubleTapeParser = Parsec ErrorData DoubleTape

mainSym :: Symbol -> TextParser a -> TextParser Tape
mainSym res = fmap (const [res])

symVariants :: Symbol -> [Symbol] -> TextParser Tape
symVariants main alts = mainSym main (choice (fmap chunk alts))

symForm :: Symbol -> [Symbol] -> TextParser Tape
symForm main alts = symVariants main (main : alts)

parseTape :: [TextParser Tape] -> TextParser Tape
parseTape parsers = fmap mconcat (manyTill (choice parsers) eof)

wordInitial :: DoubleTape -> Bool
wordInitial tape = case tapeBefore tape of
  [] -> True
  (" " : _) -> True
  _ -> False

wordInitially :: DoubleTapeParser ()
wordInitially = do
  let expectedEmpty = ME.Tokens ("" :| [])
      expectedSpace = ME.Tokens (" " :| [])
  state <- getParserState
  if wordInitial (stateInput state)
    then pure ()
    else failure Nothing (Set.fromList [expectedEmpty, expectedSpace])
  
translateTape :: DoubleTapeParser Tape -> Tape -> Tape
translateTape parser = unwrap . runParser translator name . doubleTape
  where
    unwrap = fromRight undefined
    name = "tape translator"
    translator = 
      let parseCurr = do
            head <- try parser <|> fmap (: []) anySingle
            tail <- translator
            return (head ++ tail)
      in fmap (const []) eof <|> parseCurr

shiftTape :: [DoubleTapeParser Tape] -> Tape -> [Tape]
shiftTape shifts initial = forms
  where
    folder (last, forms) shift =
      let last' = translateTape shift last
      in if last' == last then (last, forms) else (last', last' : forms)
    (_, forms) = foldl' folder (initial, [initial]) shifts
