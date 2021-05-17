module Langevo.PGmc
  ( phonemeA
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
  , parse
  , fromPIE
  , pieShifts
  ) where

import qualified Langevo.PIE as PIE
import Text.Megaparsec (chunk, single, choice)
import Text.Megaparsec.Char (space1)
import Langevo.Parse
  ( symForm
  , parseTape
  , TextParser
  , Tape
  , Symbol
  , mainSym
  , symVariants
  , translateTape
  , TapeParser
  , shiftTape
  )

fromPIE :: Tape -> Tape
fromPIE = head . shiftTape pieShifts

pieShifts :: [TapeParser Tape]
pieShifts = shifts
  where
    shifts :: [TapeParser Tape]
    shifts = [centumShift]
    centumShift :: TapeParser Tape
    centumShift =
      let table =
            [ (PIE.phonemeKj, PIE.phonemeK)
            , (PIE.phonemeGj, PIE.phonemeG)
            , (PIE.phonemeGjh, PIE.phonemeGh)
            ]
      in choice (fmap (\(inp, outp) -> fmap (const [outp]) (single inp)) table)

parse :: TextParser Tape
parse = parseTape parsers
  where
    parsers :: [TextParser Tape]
    parsers =
      [ parseWj
      , parseOu, parseOi, parseE1u, parseE1i
      , parseAu, parseAi, parseEu, parseIu
      , parseUun, parseUn, parseU, parseO
      , parseOon, parseOn, parseOo, parseO
      , parseAan, parseAn, parseAa, parseA
      , parseEe, parseE, parseE1, parseE2
      , parseIin, parseIn, parseIi, parseI
      , parseP, parseB, parseF, parseM
      , parseT, parseD, parseTh
      , parseS, parseZ, parseN, parseL, parseR
      , parseJ
      , parseKw, parseGw, parseHw, parseW
      , parseK, parseG, parseH
      , parseSpace
      ]

    parseA :: TextParser Tape
    parseA = symForm phonemeA []
    parseAn :: TextParser Tape
    parseAn = symForm phonemeAn ["ą"]
    parseAa :: TextParser Tape
    parseAa = symForm phonemeAa ["ā"]
    parseAan :: TextParser Tape
    parseAan = symForm phonemeAan ["ą̄", "ą̄", "ą̄"]
    parseO :: TextParser Tape
    parseO = symForm phonemeO ["ō"]
    parseOn :: TextParser Tape
    parseOn = symForm phonemeOn ["ǭ", "ǭ", "ǭ"]
    parseOo :: TextParser Tape
    parseOo = symForm phonemeOo ["ô"]
    parseOon :: TextParser Tape
    parseOon = symForm phonemeOon ["ǫ̂", "ǫ̂", "ǫ̂"]
    parseU :: TextParser Tape
    parseU = symForm phonemeU []
    parseUn :: TextParser Tape
    parseUn = symForm phonemeUn ["ų"]
    parseUu :: TextParser Tape
    parseUu = symForm phonemeUu ["ū"]
    parseUun :: TextParser Tape
    parseUun = symForm phonemeUun ["ų̄", "ų̄", "ų̄"]
    parseE :: TextParser Tape
    parseE = symForm phonemeE []
    parseE1 :: TextParser Tape
    parseE1 = symVariants phonemeE1 ["ē₁", "ē₁", "ē", phonemeE1]
    parseE2 :: TextParser Tape
    parseE2 = symVariants phonemeE2 ["ē₂", phonemeE2]
    parseEe :: TextParser Tape
    parseEe = symForm phonemeEe ["ê"]
    parseI :: TextParser Tape
    parseI = symForm phonemeI []
    parseIn :: TextParser Tape
    parseIn = symForm phonemeIn ["į"]
    parseIi :: TextParser Tape
    parseIi = symForm phonemeIi ["ī"]
    parseIin :: TextParser Tape
    parseIin = symForm phonemeIin [ "į̄", "į̄", "į̄" ]
    parseAu :: TextParser Tape
    parseAu = symForm phonemeAu []
    parseAi :: TextParser Tape
    parseAi = symForm phonemeAi []
    parseEu :: TextParser Tape
    parseEu = symForm phonemeEu []
    parseIu :: TextParser Tape
    parseIu = symForm phonemeIu []
    parseOu :: TextParser Tape
    parseOu = symForm phonemeOu ["ōu"]
    parseOi :: TextParser Tape
    parseOi = symForm phonemeOi ["ōi"]
    parseE1u :: TextParser Tape
    parseE1u = symForm phonemeE1u ["ēu", "ē₁u", "ē₁u"]
    parseE1i :: TextParser Tape
    parseE1i = symForm phonemeE1i ["ēi", "ē₁i", "ē₁i"]
    parseP :: TextParser Tape
    parseP = symForm phonemeP []
    parseB :: TextParser Tape
    parseB = symForm phonemeB ["ƀ", "β"]
    parseF :: TextParser Tape
    parseF = symForm phonemeF ["ɸ"]
    parseM :: TextParser Tape
    parseM = symForm phonemeM []
    parseT :: TextParser Tape
    parseT = symForm phonemeT []
    parseD :: TextParser Tape
    parseD = symForm phonemeD ["đ", "ð"]
    parseTh :: TextParser Tape
    parseTh = symForm phonemeTh ["θ"]
    parseS :: TextParser Tape
    parseS = symForm phonemeS []
    parseZ :: TextParser Tape
    parseZ = symForm phonemeZ []
    parseN :: TextParser Tape
    parseN = symForm phonemeN []
    parseL :: TextParser Tape
    parseL = symForm phonemeL []
    parseR :: TextParser Tape
    parseR = symForm phonemeR []
    parseJ :: TextParser Tape
    parseJ = symForm phonemeJ []
    parseK :: TextParser Tape
    parseK = symForm phonemeK []
    parseG :: TextParser Tape
    parseG = symForm phonemeG ["ǥ", "ɣ"]
    parseH :: TextParser Tape
    parseH = symForm phonemeH []
    parseKw :: TextParser Tape
    parseKw = symForm phonemeKw []
    parseGw :: TextParser Tape
    parseGw = symForm phonemeGw []
    parseHw :: TextParser Tape
    parseHw = symForm phonemeHw []
    parseW :: TextParser Tape
    parseW = symForm phonemeW []
    parseWj :: TextParser Tape
    parseWj = fmap (const [phonemeW, phonemeJ]) (chunk "wj")

    parseSpace :: TextParser Tape
    parseSpace = mainSym punctSpace space1 

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
phonemeT = "p"
phonemeD :: Symbol
phonemeD = "b"
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
