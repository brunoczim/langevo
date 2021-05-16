module Langevo.PGmc
  ( Phoneme
  , Word
  , fromPIE
  , phonemeA
  , phonemeAa
  , phonemeO
  , phonemeOo
  , phonemeU
  , phonemeUu
  , phonemeE
  , phonemeE1
  , phonemeE2
  , phonemeEe
  , phonemeI
  , phonemeIi
  , phonemeP
  , phonemeB
  , phonemeF
  , phonemeM
  , phonemeT
  , phonemeD
  , phonemeTh
  , phonemeS
  , phonemeZ
  , phonemeN
  , phonemeL
  , phonemeR
  , phonemeJ
  , phonemeK
  , phonemeG
  , phonemeH
  , phonemeKw
  , phonemeGw
  , phonemeHw
  , phonemeW
  ) where

import Data.Maybe (fromMaybe, maybeToList)
import qualified Langevo.PIE as PIE
import Data.Foldable (foldl')
import Text.Megaparsec
  ( Parsec
  , MonadParsec (eof, lookAhead)
  , parseError
  , (<|>)
  , customFailure
  , choice
  , chunk
  , single, many, runParser, ParseErrorBundle
  )
import Text.Megaparsec.Char (space, space1)
import Data.Text (Text)
import Prelude hiding (Word)
import Data.Void (Void)

type Phoneme = Text
type Word = [Phoneme]
type ErrorData = Void
type ShiftParser s = Parsec ErrorData s

fromPIE :: PIE.Word -> Word
fromPIE word = mconcat (foldl' (flip ($)) word shifts)
  where
    shifts :: [PIE.Word -> PIE.Word]
    shifts =
      [ centumShift
      , sonEpenthesis
      , dentalEpenthesis
      , consonShortening
      , overlonging
      --, cowgill
      ]
    centumShift :: PIE.Word -> PIE.Word
    centumShift =
      let shift ph
            | ph == PIE.phonemeKj = PIE.phonemeK
            | ph == PIE.phonemeGj = PIE.phonemeG
            | ph == PIE.phonemeGjh = PIE.phonemeGh
            | otherwise = ph
      in fmap (fmap shift)
    sonEpenthesis :: PIE.Word -> PIE.Word
    sonEpenthesis =
      let shift morpheme = do
            ph <- morpheme
            let answer
                  | ph == PIE.phonemeMSyl = [PIE.phonemeU, PIE.phonemeM]
                  | ph == PIE.phonemeMSylAcc = [PIE.phonemeUAcc, PIE.phonemeM]
                  | ph == PIE.phonemeNSyl = [PIE.phonemeU, PIE.phonemeN]
                  | ph == PIE.phonemeNSylAcc = [PIE.phonemeUAcc, PIE.phonemeN]
                  | ph == PIE.phonemeRSyl = [PIE.phonemeU, PIE.phonemeR]
                  | ph == PIE.phonemeRSylAcc = [PIE.phonemeUAcc, PIE.phonemeR]
                  | ph == PIE.phonemeLSyl = [PIE.phonemeU, PIE.phonemeL]
                  | ph == PIE.phonemeLSylAcc = [PIE.phonemeUAcc, PIE.phonemeL]
                  | otherwise = [ph]
            answer
      in fmap shift
    dentalEpenthesis :: PIE.Word -> PIE.Word
    dentalEpenthesis [last] = [last]
    dentalEpenthesis (curr : (right : suffix) : tail)
      | isDental left && isDental right = currAnswer : answerTail
      | otherwise = curr : dentalEpenthesis ((right : suffix) : tail)
      where
        isDental = (`elem` [PIE.phonemeD, PIE.phonemeT])
        prefix = init curr
        left = last curr
        currAnswer = prefix ++ [PIE.phonemeS]
        answerTail = dentalEpenthesis ((PIE.phonemeS : suffix) : tail)
    consonShortening :: PIE.Word -> PIE.Word
    consonShortening word =
      let shortVowels =
            [ PIE.phonemeA, PIE.phonemeAAcc
            , PIE.phonemeE, PIE.phonemeEAcc
            , PIE.phonemeI, PIE.phonemeIAcc
            , PIE.phonemeO, PIE.phonemeOAcc
            , PIE.phonemeU, PIE.phonemeUAcc
            ]
          iterMorphemes _ _ [] = []
          iterMorphemes prev gem (head : tail) =
            let (prev', gem', head') = iterMorpheme prev gem head
            in head' : iterMorphemes prev' gem' tail
          iterMorpheme prev gem [] = (prev, gem, [])
          iterMorpheme prev gem (curr : tail)
            | Just curr == gem && prevShortens = (prev', gem', rest)
            | otherwise = (prev', gem', curr : rest)
            where
              prevShortens = maybe True (`notElem` shortVowels) prev
              (prev', gem', rest) = iterMorpheme gem (Just curr) tail
        in iterMorphemes Nothing Nothing word
    overlonging :: PIE.Word -> PIE.Word
    overlonging [] = []
    overlonging word =
      let longVowels =
            [ PIE.phonemeAa, PIE.phonemeAaAcc
            , PIE.phonemeEe, PIE.phonemeEeAcc
            , PIE.phonemeIi, PIE.phonemeIiAcc
            , PIE.phonemeOo, PIE.phonemeOoAcc
            , PIE.phonemeUu, PIE.phonemeUuAcc
            ]
          initMorphemes = init word
          lastMorpheme = last word
          initPhonemes = init lastMorpheme
          lastPhoneme = last lastMorpheme
          overlongLast
            | lastPhoneme == PIE.phonemeAa = "â"
            | lastPhoneme == PIE.phonemeEe = phonemeEe
            | lastPhoneme == PIE.phonemeIi = "î"
            | lastPhoneme == PIE.phonemeOo = phonemeOo
            | lastPhoneme == PIE.phonemeUu = "û"
          word'
            | null lastMorpheme || lastPhoneme `notElem` longVowels = word
            | otherwise = initMorphemes ++ [initPhonemes ++ [overlongLast]]
      in word'
    {-
    cowgill :: PIE.Word -> PIE.Word
    cowgill word =
      let sonorants =
            [ PIE.phonemeM
            , PIE.phonemeN
            , PIE.phonemeR
            , PIE.phonemeL
            ]
          targetsH = [PIE.phonemeH2, PIE.phonemeH3]
          iterMorphemes _ _ [] = []
          iterMorphemes prev curr (head : tail) =
            let (prev', curr', head') = iterMorpheme prev curr head
            in head' : iterMorphemes prev' curr' tail
          iterMorpheme prev curr [] = (Nothing, prev, curr, [])
          iterMorpheme prev curr (next : tail)
            | True = (prev', curr', next', rest)
            | otherwise = (prev', curr', next', rest)
            where
              (prev', curr', next', rest) = iterMorpheme curr (Just next) tail
      in undefined
    -}

phonemeA :: Phoneme
phonemeA = "a"
phonemeAa :: Phoneme
phonemeAa = "ā"
phonemeO :: Phoneme
phonemeO = "ō"
phonemeOo :: Phoneme
phonemeOo = "ô"
phonemeU :: Phoneme
phonemeU = "u"
phonemeUu :: Phoneme
phonemeUu = "ū"
phonemeE :: Phoneme
phonemeE = "e"
phonemeE1 :: Phoneme
phonemeE1 = "ē"
phonemeE2 :: Phoneme
phonemeE2 = "ē₂"
phonemeEe :: Phoneme
phonemeEe = "ê"
phonemeI :: Phoneme
phonemeI = "i"
phonemeIi :: Phoneme
phonemeIi = "ī"
phonemeP :: Phoneme
phonemeP = "p"
phonemeB :: Phoneme
phonemeB = "b"
phonemeF :: Phoneme
phonemeF = "f"
phonemeM :: Phoneme
phonemeM = "m"
phonemeT :: Phoneme
phonemeT = "p"
phonemeD :: Phoneme
phonemeD = "b"
phonemeTh :: Phoneme
phonemeTh = "þ"
phonemeS :: Phoneme
phonemeS = "s"
phonemeZ :: Phoneme
phonemeZ = "z"
phonemeN :: Phoneme
phonemeN = "n"
phonemeL :: Phoneme
phonemeL = "l"
phonemeR :: Phoneme
phonemeR = "r"
phonemeJ :: Phoneme
phonemeJ = "j"
phonemeK :: Phoneme
phonemeK = "k"
phonemeG :: Phoneme
phonemeG = "g"
phonemeH :: Phoneme
phonemeH = "h"
phonemeKw :: Phoneme
phonemeKw = "kw"
phonemeGw :: Phoneme
phonemeGw = "gw"
phonemeHw :: Phoneme
phonemeHw = "hw"
phonemeW :: Phoneme
phonemeW = "w"
