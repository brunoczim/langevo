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
import Text.Megaparsec
  ( chunk
  , single
  , choice
  , (<|>)
  , oneOf
  , noneOf
  , MonadParsec (eof), optional)
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
import Data.Maybe (maybeToList)

fromPIE :: Tape -> Tape
fromPIE = head . shiftTape pieShifts

pieShifts :: [TapeParser Tape]
pieShifts = shifts
  where
    shifts :: [TapeParser Tape]
    shifts =
      [ centumShift
      , sonEpenthesis
      , dentalEpenthesis
      , firstDentalSimpl
      , secondDentalSimpl
      , geminateSimpl
      , overlonging
      , cowgillsLaw
      , h1LossBeforeV
      , h2LossBeforeE
      , h2LossBeforeOther
      , h3LossBeforeE
      , h3LossBeforeOther
      , hLossAfterV
      , hVocaliz
      , labiovelarMerge
      , labiovelarDelab
      ]
    centumShift :: TapeParser Tape
    centumShift =
      let table =
            [ (PIE.phonemeKj, PIE.phonemeK)
            , (PIE.phonemeGj, PIE.phonemeG)
            , (PIE.phonemeGjh, PIE.phonemeGh)
            ]
      in choice (fmap (\(inp, outp) -> fmap (const [outp]) (single inp)) table)
    sonEpenthesis :: TapeParser Tape
    sonEpenthesis =
      let sonorants =
            [ (PIE.phonemeMSyl, PIE.phonemeU, PIE.phonemeM)
            , (PIE.phonemeNSyl, PIE.phonemeU, PIE.phonemeN)
            , (PIE.phonemeRSyl, PIE.phonemeU, PIE.phonemeR)
            , (PIE.phonemeLSyl, PIE.phonemeU, PIE.phonemeL)
            , (PIE.phonemeMSylAcc, PIE.phonemeUAcc, PIE.phonemeM)
            , (PIE.phonemeNSylAcc, PIE.phonemeUAcc, PIE.phonemeN)
            , (PIE.phonemeRSylAcc, PIE.phonemeUAcc, PIE.phonemeR)
            , (PIE.phonemeLSylAcc, PIE.phonemeUAcc, PIE.phonemeL)
            ]
          innerParse (inp, out0, out1) =
            let replace = [out0, out1]
            in fmap (const replace) (single inp)
      in choice (fmap innerParse sonorants)
    dentalEpenthesis :: TapeParser Tape 
    dentalEpenthesis = do
      let parseDental = single PIE.phonemeT
            <|> single PIE.phonemeD
            <|> single PIE.phonemeDh
      first <- parseDental
      single PIE.punctHyphen
      second <- parseDental
      return [first, PIE.phonemeS, second]
    firstDentalSimpl :: TapeParser Tape
    firstDentalSimpl = do
      let parseDental = single PIE.phonemeT
            <|> single PIE.phonemeD
            <|> single PIE.phonemeDh
      parseDental
      single PIE.phonemeS
      parseDental
      return [PIE.phonemeT, PIE.phonemeS]
    secondDentalSimpl :: TapeParser Tape
    secondDentalSimpl = do
      single PIE.phonemeT
      single PIE.phonemeS
      return [PIE.phonemeS, PIE.phonemeS]
    geminateSimpl :: TapeParser Tape
    geminateSimpl = do
      let shortVowels =
            [ PIE.phonemeA, PIE.phonemeAAcc
            , PIE.phonemeE, PIE.phonemeEAcc
            , PIE.phonemeI, PIE.phonemeIAcc
            , PIE.phonemeO, PIE.phonemeOAcc
            , PIE.phonemeU, PIE.phonemeUAcc
            ]
          consonants =
            [ PIE.phonemeM, PIE.phonemeN, PIE.phonemeR, PIE.phonemeL
            , PIE.phonemeW, PIE.phonemeY
            , PIE.phonemeP, PIE.phonemeB, PIE.phonemeBh
            , PIE.phonemeT, PIE.phonemeD, PIE.phonemeDh
            , PIE.phonemeK, PIE.phonemeG, PIE.phonemeGh
            , PIE.phonemeKw, PIE.phonemeGw, PIE.phonemeGwh
            , PIE.phonemeS, PIE.phonemeH1, PIE.phonemeH2, PIE.phonemeH3
            ]
      preceeding <- noneOf shortVowels
      consonant <- oneOf consonants
      single consonant
      return [preceeding, consonant]
    overlonging :: TapeParser Tape
    overlonging = do
      let longVowelTable =
            [ (PIE.phonemeAa, "â")
            , (PIE.phonemeEe, "ê")
            , (PIE.phonemeIi, "î")
            , (PIE.phonemeOo, "ô")
            , (PIE.phonemeUu, "û")
            , (PIE.phonemeAaAcc, "ấ")
            , (PIE.phonemeEeAcc, "ế")
            , (PIE.phonemeIiAcc, "î́")
            , (PIE.phonemeOoAcc, "ố")
            , (PIE.phonemeUuAcc, "û́")
            ]
          makeLongParser (inp, outp) = fmap (const outp) (single inp) 
      overlonged <- choice (fmap makeLongParser longVowelTable)
      end <- chunk [PIE.punctSpace] <|> fmap (const []) eof 
      return (overlonged : end)
    cowgillsLaw :: TapeParser Tape
    cowgillsLaw = do
      let sonorants =
            [ PIE.phonemeM
            , PIE.phonemeN
            , PIE.phonemeR
            , PIE.phonemeL
            ]
      sonorant <- choice (fmap single sonorants)
      laryngeal <- choice (fmap single [PIE.phonemeH2, PIE.phonemeH3])
      labiovelarSon <- single PIE.phonemeW
      return [sonorant, PIE.phonemeG, labiovelarSon]
    h1LossBeforeV :: TapeParser Tape
    h1LossBeforeV = do
      let vowels =
            [ PIE.phonemeA, PIE.phonemeAAcc, PIE.phonemeAa, PIE.phonemeAaAcc
            , "â", "ấ"
            , PIE.phonemeE, PIE.phonemeEAcc, PIE.phonemeEe, PIE.phonemeEeAcc
            , "ê", "ế"
            , PIE.phonemeI, PIE.phonemeIAcc, PIE.phonemeIi, PIE.phonemeIiAcc
            , "î", "î́"
            , PIE.phonemeO, PIE.phonemeOAcc, PIE.phonemeOo, PIE.phonemeOoAcc
            , "ô", "ố"
            , PIE.phonemeU, PIE.phonemeUAcc, PIE.phonemeUu, PIE.phonemeUuAcc
            , "û", "û́"
            ]
      single PIE.phonemeH1
      vowel <- choice (fmap single vowels)
      return [vowel]
    h2LossBeforeE :: TapeParser Tape
    h2LossBeforeE = do
      let vowelTable =
            [ (PIE.phonemeE, PIE.phonemeA)
            , (PIE.phonemeEAcc, PIE.phonemeAAcc)
            , (PIE.phonemeEe, PIE.phonemeAa)
            , (PIE.phonemeEeAcc, PIE.phonemeAaAcc)
            , ("ê", "â")
            , ("ế", "ấ")
            ]
          makeVowelParser (inp, outp) = fmap (const outp) (single inp)
      single PIE.phonemeH2
      newVowel <- choice (fmap makeVowelParser vowelTable)
      return [newVowel]
    h2LossBeforeOther :: TapeParser Tape
    h2LossBeforeOther = do
      let vowels =
            [ PIE.phonemeA, PIE.phonemeAAcc, PIE.phonemeAa, PIE.phonemeAaAcc
            , "â", "ấ"
            , PIE.phonemeI, PIE.phonemeIAcc, PIE.phonemeIi, PIE.phonemeIiAcc
            , "î", "î́"
            , PIE.phonemeO, PIE.phonemeOAcc, PIE.phonemeOo, PIE.phonemeOoAcc
            , "ô", "ố"
            , PIE.phonemeU, PIE.phonemeUAcc, PIE.phonemeUu, PIE.phonemeUuAcc
            , "û", "û́"
            ]
      single PIE.phonemeH2
      vowel <- choice (fmap single vowels)
      return [vowel]
    h3LossBeforeE :: TapeParser Tape
    h3LossBeforeE = do
      let vowelTable =
            [ (PIE.phonemeE, PIE.phonemeO)
            , (PIE.phonemeEAcc, PIE.phonemeOAcc)
            , (PIE.phonemeEe, PIE.phonemeOo)
            , (PIE.phonemeEeAcc, PIE.phonemeOoAcc)
            , ("ê", "ô")
            , ("ế", "ố")
            ]
          makeVowelParser (inp, outp) = fmap (const outp) (single inp)
      single PIE.phonemeH3
      newVowel <- choice (fmap makeVowelParser vowelTable)
      return [newVowel]
    h3LossBeforeOther :: TapeParser Tape
    h3LossBeforeOther = do
      let vowels =
            [ PIE.phonemeA, PIE.phonemeAAcc, PIE.phonemeAa, PIE.phonemeAaAcc
            , "â", "ấ"
            , PIE.phonemeI, PIE.phonemeIAcc, PIE.phonemeIi, PIE.phonemeIiAcc
            , "î", "î́"
            , PIE.phonemeO, PIE.phonemeOAcc, PIE.phonemeOo, PIE.phonemeOoAcc
            , "ô", "ố"
            , PIE.phonemeU, PIE.phonemeUAcc, PIE.phonemeUu, PIE.phonemeUuAcc
            , "û", "û́"
            ]
      single PIE.phonemeH3
      vowel <- choice (fmap single vowels)
      return [vowel]
    hLossAfterV :: TapeParser Tape
    hLossAfterV = do
      let vowelTable =
            [ (PIE.phonemeA, PIE.phonemeAa)
            , (PIE.phonemeAa, PIE.phonemeAa)
            , (PIE.phonemeAAcc, PIE.phonemeAaAcc)
            , (PIE.phonemeAaAcc, PIE.phonemeAaAcc)
            , (PIE.phonemeE, PIE.phonemeEe)
            , (PIE.phonemeEe, PIE.phonemeEe)
            , (PIE.phonemeEAcc, PIE.phonemeEeAcc)
            , (PIE.phonemeEeAcc, PIE.phonemeEeAcc)
            , (PIE.phonemeI, PIE.phonemeIi)
            , (PIE.phonemeIi, PIE.phonemeIi)
            , (PIE.phonemeIAcc, PIE.phonemeIiAcc)
            , (PIE.phonemeIiAcc, PIE.phonemeIiAcc)
            , (PIE.phonemeO, PIE.phonemeOo)
            , (PIE.phonemeOo, PIE.phonemeOo)
            , (PIE.phonemeOAcc, PIE.phonemeOoAcc)
            , (PIE.phonemeOoAcc, PIE.phonemeOoAcc)
            , (PIE.phonemeU, PIE.phonemeUu)
            , (PIE.phonemeUu, PIE.phonemeUu)
            , (PIE.phonemeUAcc, PIE.phonemeUuAcc)
            , (PIE.phonemeUuAcc, PIE.phonemeUuAcc)
            ]
          makeVowelParser (inp, outp) = fmap (const outp) (single inp)
          laryngeals = [PIE.phonemeH1, PIE.phonemeH2, PIE.phonemeH3] 
      newVowel <- choice (fmap makeVowelParser vowelTable)
      choice (fmap single laryngeals)
      return [newVowel]
    hVocaliz :: TapeParser Tape
    hVocaliz = do
      let laryngeals = [PIE.phonemeH1, PIE.phonemeH2, PIE.phonemeH3] 
      choice (fmap single laryngeals)
      return ["ə"]
    labiovelarMerge :: TapeParser Tape
    labiovelarMerge = do
      let velarTable =
            [ (PIE.phonemeK, PIE.phonemeKw)
            , (PIE.phonemeG, PIE.phonemeGw)
            , (PIE.phonemeGh, PIE.phonemeGwh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
      labiovelar <- choice (fmap makeVelarParser velarTable)
      single PIE.phonemeW
      return [labiovelar]
    labiovelarDelab :: TapeParser Tape
    labiovelarDelab = do
      let velarTable =
            [ (PIE.phonemeKw,PIE.phonemeK)
            , (PIE.phonemeGw,PIE.phonemeG)
            , (PIE.phonemeGwh,PIE.phonemeGh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
          vowels =
            [ PIE.phonemeU
            , PIE.phonemeUAcc
            , PIE.phonemeUu
            , PIE.phonemeUuAcc
            ]
      velar <- choice (fmap makeVelarParser velarTable)
      vowel <- choice (fmap single vowels)
      maybeN <- fmap maybeToList (optional (single PIE.phonemeN))
      plosive <- single PIE.phonemeT
      return ([velar, vowel] ++ maybeN ++ [plosive])
    
      
        

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
