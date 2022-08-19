module Langevo.PieToPGmc
  ( phonemeM
  , phonemeN
  , phonemeP
  , phonemeB
  , phonemeBh
  , phonemeT
  , phonemeD
  , phonemeDh
  , phonemeKj
  , phonemeGj
  , phonemeGjh
  , phonemeK
  , phonemeG
  , phonemeGh
  , phonemeKw
  , phonemeGw
  , phonemeGwh
  , phonemeS
  , phonemeH1
  , phonemeH2
  , phonemeH3
  , phonemeH
  , phonemeR
  , phonemeL
  , phonemeY
  , phonemeW
  , phonemeA
  , phonemeE
  , phonemeI
  , phonemeO
  , phonemeU
  , phonemeAAcc
  , phonemeEAcc
  , phonemeIAcc
  , phonemeOAcc
  , phonemeUAcc
  , phonemeAa
  , phonemeEe
  , phonemeIi
  , phonemeOo
  , phonemeUu
  , phonemeAaAcc
  , phonemeEeAcc
  , phonemeIiAcc
  , phonemeOoAcc
  , phonemeUuAcc
  , phonemeRSyl
  , phonemeLSyl
  , phonemeMSyl
  , phonemeNSyl
  , phonemeRSylAcc
  , phonemeLSylAcc
  , phonemeMSylAcc
  , phonemeNSylAcc
  , punctSpace
  , punctHyphen
  , phonemeA
  , phonemeAa
  , phonemeAn
  , phonemeAan
  , phonemeO
  , phonemeOo
  , phonemeOn
  , phonemeOon
  , phonemeU
  , phonemeUu
  , phonemeUn
  , phonemeUun
  , phonemeE
  , phonemeE1
  , phonemeE2
  , phonemeEe
  , phonemeI
  , phonemeIi
  , phonemeIn
  , phonemeIin
  , phonemeAu
  , phonemeAi
  , phonemeEu
  , phonemeIu
  , phonemeOu
  , phonemeOi
  , phonemeE1u
  , phonemeE1i
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
  , punctSpace
  ) where

import qualified Langevo.Pie as Pie
import qualified Langevo.PGmc as PGmc


phonemeM :: Symbol
phonemeM = "m"
phonemeN :: Symbol
phonemeN = "n"
phonemeP :: Symbol
phonemeP = "p"
phonemeB :: Symbol
phonemeB = "b"
phonemeBh :: Symbol
phonemeBh = "bʰ"
phonemeT :: Symbol
phonemeT = "t"
phonemeD :: Symbol
phonemeD = "d"
phonemeDh :: Symbol
phonemeDh = "dʰ"
phonemeKj :: Symbol
phonemeKj = "ḱ"
phonemeGj :: Symbol
phonemeGj = "ǵ"
phonemeGjh :: Symbol
phonemeGjh = "ǵʰ"
phonemeK :: Symbol
phonemeK = "k"
phonemeG :: Symbol
phonemeG = "g"
phonemeGh :: Symbol
phonemeGh = "gʰ"
phonemeKw :: Symbol
phonemeKw = "kʷ"
phonemeGw :: Symbol
phonemeGw = "gʷ"
phonemeGwh :: Symbol
phonemeGwh = "gʷʰ"
phonemeS :: Symbol
phonemeS = "s"
phonemeH1 :: Symbol
phonemeH1 = "h₁"
phonemeH2 :: Symbol
phonemeH2 = "h₂"
phonemeH3 :: Symbol
phonemeH3 = "h₃"
phonemeH :: Symbol
phonemeH = "H"
phonemeR :: Symbol
phonemeR = "r"
phonemeL :: Symbol
phonemeL = "l"
phonemeY :: Symbol
phonemeY = "y"
phonemeW :: Symbol
phonemeW = "w"
phonemeA :: Symbol
phonemeA = "a"
phonemeE :: Symbol
phonemeE = "e"
phonemeI :: Symbol
phonemeI = "i"
phonemeO :: Symbol
phonemeO = "o"
phonemeU :: Symbol
phonemeU = "u"
phonemeAAcc :: Symbol
phonemeAAcc = "á"
phonemeEAcc:: Symbol
phonemeEAcc = "é"
phonemeIAcc :: Symbol
phonemeIAcc = "í"
phonemeOAcc :: Symbol
phonemeOAcc = "ó"
phonemeUAcc :: Symbol
phonemeUAcc = "ú"
phonemeAa :: Symbol
phonemeAa = "ā"
phonemeEe :: Symbol
phonemeEe = "ē"
phonemeIi :: Symbol
phonemeIi = "ī"
phonemeOo :: Symbol
phonemeOo = "ō"
phonemeUu :: Symbol
phonemeUu = "ū"
phonemeAaAcc :: Symbol
phonemeAaAcc = "ā́"
phonemeEeAcc :: Symbol
phonemeEeAcc = "ḗ"
phonemeIiAcc :: Symbol
phonemeIiAcc = "ī́"
phonemeOoAcc :: Symbol
phonemeOoAcc = "ṓ"
phonemeUuAcc :: Symbol
phonemeUuAcc = "ū́"
phonemeRSyl :: Symbol
phonemeRSyl = "r̥"
phonemeLSyl :: Symbol
phonemeLSyl = "l̥"
phonemeMSyl :: Symbol
phonemeMSyl = "m̥"
phonemeNSyl :: Symbol
phonemeNSyl = "n̥"
phonemeRSylAcc :: Symbol
phonemeRSylAcc = "ŕ̥"
phonemeLSylAcc :: Symbol
phonemeLSylAcc = "ĺ̥"
phonemeMSylAcc :: Symbol
phonemeMSylAcc = "ḿ̥"
phonemeNSylAcc :: Symbol
phonemeNSylAcc = "ń̥"
punctSpace :: Symbol
punctSpace = " "
punctHyphen :: Symbol
punctHyphen = "-"
phonemeA :: Symbol
phonemeA = "a"
phonemeAn :: Symbol
phonemeAn = "ą"
phonemeAa :: Symbol
phonemeAa = "ā"
phonemeAan :: Symbol
phonemeAan = "ą̄"
phonemeO :: Symbol
phonemeO = "ō"
phonemeOn :: Symbol
phonemeOn = "ǭ"
phonemeOo :: Symbol
phonemeOo = "ô"
phonemeOon :: Symbol
phonemeOon = "ǫ̂"
phonemeU :: Symbol
phonemeU = "u"
phonemeUn :: Symbol
phonemeUn = "ų"
phonemeUu :: Symbol
phonemeUu = "ū"
phonemeUun :: Symbol
phonemeUun = "ų̄"
phonemeE :: Symbol
phonemeE = "e"
phonemeE1 :: Symbol
phonemeE1 = "ē"
phonemeE2 :: Symbol
phonemeE2 = "ē₂"
phonemeEe :: Symbol
phonemeEe = "ê"
phonemeI :: Symbol
phonemeI = "i"
phonemeIn :: Symbol
phonemeIn = "į"
phonemeIi :: Symbol
phonemeIi = "ī"
phonemeIin :: Symbol
phonemeIin = "į̄"
phonemeAu :: Symbol
phonemeAu = "au"
phonemeAi :: Symbol
phonemeAi = "ai"
phonemeEu :: Symbol
phonemeEu = "eu"
phonemeIu :: Symbol
phonemeIu = "iu"
phonemeOu :: Symbol
phonemeOu = "ōu"
phonemeOi :: Symbol
phonemeOi = "ōi"
phonemeE1u :: Symbol
phonemeE1u = "ēu"
phonemeE1i :: Symbol
phonemeE1i = "ēi"
phonemeP :: Symbol
phonemeP = "p"
phonemeB :: Symbol
phonemeB = "b"
phonemeF :: Symbol
phonemeF = "f"
phonemeM :: Symbol
phonemeM = "m"
phonemeT :: Symbol
phonemeT = "t"
phonemeD :: Symbol
phonemeD = "d"
phonemeTh :: Symbol
phonemeTh = "þ"
phonemeS :: Symbol
phonemeS = "s"
phonemeZ :: Symbol
phonemeZ = "z"
phonemeN :: Symbol
phonemeN = "n"
phonemeL :: Symbol
phonemeL = "l"
phonemeR :: Symbol
phonemeR = "r"
phonemeJ :: Symbol
phonemeJ = "j"
phonemeK :: Symbol
phonemeK = "k"
phonemeG :: Symbol
phonemeG = "g"
phonemeH :: Symbol
phonemeH = "h"
phonemeKw :: Symbol
phonemeKw = "kw"
phonemeGw :: Symbol
phonemeGw = "gw"
phonemeHw :: Symbol
phonemeHw = "hw"
phonemeW :: Symbol
phonemeW = "w"
punctSpace :: Symbol
punctSpace = " "
