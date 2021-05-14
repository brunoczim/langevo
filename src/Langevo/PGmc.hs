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

import qualified Langevo.PIE as PIE
import Data.Foldable (foldr')
import Data.Text (Text)
import Prelude hiding (Word)

type Phoneme = Text
type Word = [Phoneme]

fromPIE :: PIE.Word -> Word
fromPIE text = foldr' ($) (mconcat text) shifts
  where
    shifts :: [Word -> Word]
    shifts = [centumShift]
    centumShift :: Word -> Word
    centumShift =
      let shift ph
            | ph == PIE.phonemeKj = PIE.phonemeK
            | ph == PIE.phonemeGj = PIE.phonemeG
            | ph == PIE.phonemeGjh = PIE.phonemeGh
            | otherwise = ph
      in fmap shift

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
