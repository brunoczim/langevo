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
  , fromPie
  , pieShifts
  ) where

import qualified Langevo.Pie as Pie
import Text.Megaparsec
  ( chunk
  , single
  , choice
  , (<|>)
  , oneOf
  , noneOf
  , manyTill_
  , manyTill
  , lookAhead
  , MonadParsec (eof)
  , optional
  , anySingleBut
  , anySingle
  )
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
  , DoubleTapeParser
  , shiftTape
  , wordInitially
  )
import Data.Maybe (maybeToList)

fromPie :: Tape -> Tape
fromPie = head . shiftTape pieShifts

pieShifts :: [DoubleTapeParser Tape]
pieShifts = shifts
  where
    shifts :: [DoubleTapeParser Tape]
    shifts =
      [ centumShift
      , sonEpenthesis
      , dentalEpenthesis
      , firstDentalSimpl
      , secondDentalSimpl
      , geminateSimpl
      , overlonging
      , cowgillsLaw
      , h2ColorNext
      , h3ColorNext
      , h2ColorPrev
      , h3ColorPrev
      , hLossBetweenV
      , hLossAfterV
      , hLossBeforeV
      , hLossInitially
      , hVocaliz
      , labiovelarMerge
      , delabBeforeU
      , delabAfterU
      , delabBeforeT
      , finalShortAccShift
      , finalShortLoss
      , preSpirantLaw
      , grimmsLawVless
      , grimmsLawVoiced
      , vernersLaw
      , accentLoss
      , initLabiovelToLabial
      , assimilateNw
      , assimilateLn
      , assimilateZm
      ]

    centumShift :: DoubleTapeParser Tape
    centumShift =
      let table =
            [ (Pie.phonemeKj, Pie.phonemeK)
            , (Pie.phonemeGj, Pie.phonemeG)
            , (Pie.phonemeGjh, Pie.phonemeGh)
            ]
      in choice (fmap (\(inp, outp) -> fmap (const [outp]) (single inp)) table)

    sonEpenthesis :: DoubleTapeParser Tape
    sonEpenthesis =
      let sonorants =
            [ (Pie.phonemeMSyl, Pie.phonemeU, Pie.phonemeM)
            , (Pie.phonemeNSyl, Pie.phonemeU, Pie.phonemeN)
            , (Pie.phonemeRSyl, Pie.phonemeU, Pie.phonemeR)
            , (Pie.phonemeLSyl, Pie.phonemeU, Pie.phonemeL)
            , (Pie.phonemeMSylAcc, Pie.phonemeUAcc, Pie.phonemeM)
            , (Pie.phonemeNSylAcc, Pie.phonemeUAcc, Pie.phonemeN)
            , (Pie.phonemeRSylAcc, Pie.phonemeUAcc, Pie.phonemeR)
            , (Pie.phonemeLSylAcc, Pie.phonemeUAcc, Pie.phonemeL)
            ]
          innerParse (inp, out0, out1) =
            let replace = [out0, out1]
            in fmap (const replace) (single inp)
      in choice (fmap innerParse sonorants)

    dentalEpenthesis :: DoubleTapeParser Tape 
    dentalEpenthesis = do
      let parseDental = single Pie.phonemeT
            <|> single Pie.phonemeD
            <|> single Pie.phonemeDh
      first <- parseDental
      single Pie.punctHyphen
      second <- parseDental
      return [first, Pie.phonemeS, second]

    firstDentalSimpl :: DoubleTapeParser Tape
    firstDentalSimpl = do
      let parseDental = single Pie.phonemeT
            <|> single Pie.phonemeD
            <|> single Pie.phonemeDh
      parseDental
      single Pie.phonemeS
      parseDental
      return [Pie.phonemeT, Pie.phonemeS]

    secondDentalSimpl :: DoubleTapeParser Tape
    secondDentalSimpl = do
      oneOf [Pie.phonemeT, Pie.phonemeD, Pie.phonemeDh]
      single Pie.phonemeS
      return [Pie.phonemeS, Pie.phonemeS]

    geminateSimpl :: DoubleTapeParser Tape
    geminateSimpl = do
      let shortVowels =
            [ Pie.phonemeA, Pie.phonemeAAcc
            , Pie.phonemeE, Pie.phonemeEAcc
            , Pie.phonemeI, Pie.phonemeIAcc
            , Pie.phonemeO, Pie.phonemeOAcc
            , Pie.phonemeU, Pie.phonemeUAcc
            ]
          consonants =
            [ Pie.phonemeM, Pie.phonemeN, Pie.phonemeR, Pie.phonemeL
            , Pie.phonemeW, Pie.phonemeY
            , Pie.phonemeP, Pie.phonemeB, Pie.phonemeBh
            , Pie.phonemeT, Pie.phonemeD, Pie.phonemeDh
            , Pie.phonemeK, Pie.phonemeG, Pie.phonemeGh
            , Pie.phonemeKw, Pie.phonemeGw, Pie.phonemeGwh
            , Pie.phonemeS, Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3
            ]
      preceeding <- noneOf shortVowels
      consonant <- oneOf consonants
      single consonant
      return [preceeding, consonant]

    overlonging :: DoubleTapeParser Tape
    overlonging = do
      let longVowelTable =
            [ (Pie.phonemeAa, "â")
            , (Pie.phonemeEe, phonemeEe)
            , (Pie.phonemeIi, "î")
            , (Pie.phonemeOo, phonemeOo)
            , (Pie.phonemeUu, "û")
            , (Pie.phonemeAaAcc, "ấ")
            , (Pie.phonemeEeAcc, "ế")
            , (Pie.phonemeIiAcc, "î́")
            , (Pie.phonemeOoAcc, "ố")
            , (Pie.phonemeUuAcc, "û́")
            ]
          makeLongParser (inp, outp) = fmap (const outp) (single inp) 
      overlonged <- choice (fmap makeLongParser longVowelTable)
      end <- chunk [Pie.punctSpace] <|> fmap (const []) eof 
      return (overlonged : end)

    cowgillsLaw :: DoubleTapeParser Tape
    cowgillsLaw = do
      let sonorants =
            [ Pie.phonemeM
            , Pie.phonemeN
            , Pie.phonemeR
            , Pie.phonemeL
            ]
      sonorant <- choice (fmap single sonorants)
      laryngeal <- choice (fmap single [Pie.phonemeH2, Pie.phonemeH3])
      labiovelarSon <- single Pie.phonemeW
      return [sonorant, Pie.phonemeG, labiovelarSon]

    makeSymbolTranslator :: (Symbol, Symbol) -> DoubleTapeParser Symbol
    makeSymbolTranslator (inp, outp) = fmap (const outp) (single inp)

    h2ColorTable :: [(Symbol, Symbol)]
    h2ColorTable =
      [ (Pie.phonemeE, Pie.phonemeA)
      , (Pie.phonemeEAcc, Pie.phonemeAAcc)
      , (Pie.phonemeEe, Pie.phonemeAa)
      , (Pie.phonemeEeAcc, Pie.phonemeAaAcc)
      , (phonemeEe, "â")
      , ("ế", "ấ")
      ]

    h3ColorTable :: [(Symbol, Symbol)]
    h3ColorTable =
      [ (Pie.phonemeE, Pie.phonemeO)
      , (Pie.phonemeEAcc, Pie.phonemeOAcc)
      , (Pie.phonemeEe, Pie.phonemeOo)
      , (Pie.phonemeEeAcc, Pie.phonemeOoAcc)
      , (phonemeEe, "ô")
      , ("ế", "ố")
      ]

    h2ColorNext :: DoubleTapeParser Tape
    h2ColorNext = do
      laryngeal <- single Pie.phonemeH2
      newVowel <- choice (fmap makeSymbolTranslator h2ColorTable)
      return [laryngeal, newVowel]

    h3ColorNext :: DoubleTapeParser Tape
    h3ColorNext = do
      laryngeal <- single Pie.phonemeH3
      newVowel <- choice (fmap makeSymbolTranslator h3ColorTable)
      return [laryngeal, newVowel]

    h2ColorPrev :: DoubleTapeParser Tape
    h2ColorPrev = do
      newVowel <- choice (fmap makeSymbolTranslator h2ColorTable)
      laryngeal <- single Pie.phonemeH2
      return [newVowel, laryngeal]

    h3ColorPrev :: DoubleTapeParser Tape
    h3ColorPrev = do
      newVowel <- choice (fmap makeSymbolTranslator h3ColorTable)
      laryngeal <- single Pie.phonemeH3
      return [newVowel, laryngeal]

    hLossBetweenV :: DoubleTapeParser Tape
    hLossBetweenV = do
      let laryngeals = [Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3] 
          vowelTable = 
            [ ( [Pie.phonemeA, Pie.phonemeAa, "â"], "â")
            , ( [Pie.phonemeAAcc, Pie.phonemeAaAcc, "ấ"], "ấ")
            , ( [Pie.phonemeE, Pie.phonemeEe, "ê"], "ê")
            , ( [Pie.phonemeEAcc, Pie.phonemeEeAcc, "ế"], "ế")
            , ( [Pie.phonemeI, Pie.phonemeIi], Pie.phonemeIi)
            , ( [Pie.phonemeIAcc, Pie.phonemeIiAcc], Pie.phonemeIiAcc)
            , ( [Pie.phonemeO, Pie.phonemeOo, "ô"], "ô")
            , ( [Pie.phonemeOAcc, Pie.phonemeOoAcc, "ố"], "ố")
            , ( [Pie.phonemeU, Pie.phonemeUu], Pie.phonemeUu)
            , ( [Pie.phonemeUAcc, Pie.phonemeUuAcc], Pie.phonemeUuAcc)
            ]
          makeVowelParser (alts, res) = do
            choice (fmap single alts)
            choice (fmap single laryngeals)
            choice (fmap single alts)
            return res
  
      newVowel <- choice (fmap makeVowelParser vowelTable)
      return [newVowel]

    hLossBeforeV :: DoubleTapeParser Tape
    hLossBeforeV = do
      let laryngeals = [Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3] 
          vowels =
            [ Pie.phonemeA, Pie.phonemeAAcc, Pie.phonemeAa, Pie.phonemeAaAcc
            , "â", "ấ"
            , Pie.phonemeE, Pie.phonemeEAcc, Pie.phonemeEe, Pie.phonemeEeAcc
            , phonemeEe, "ế"
            , Pie.phonemeI, Pie.phonemeIAcc, Pie.phonemeIi, Pie.phonemeIiAcc
            , "î", "î́"
            , Pie.phonemeO, Pie.phonemeOAcc, Pie.phonemeOo, Pie.phonemeOoAcc
            , phonemeOo, "ố", "ố"
            , Pie.phonemeU, Pie.phonemeUAcc, Pie.phonemeUu, Pie.phonemeUuAcc
            , "û", "û́"
            ]
      choice (fmap single laryngeals)
      vowel <- choice (fmap single vowels)
      return [vowel]

    hLossInitially :: DoubleTapeParser Tape
    hLossInitially = do
      let laryngeals = [Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3] 
          consonants =
            [ Pie.phonemeP, Pie.phonemeB, Pie.phonemeBh
            , Pie.phonemeT, Pie.phonemeD, Pie.phonemeDh
            , Pie.phonemeK, Pie.phonemeG, Pie.phonemeGh
            , Pie.phonemeKw, Pie.phonemeGw, Pie.phonemeGwh
            , Pie.phonemeS, Pie.phonemeY, Pie.phonemeW
            , Pie.phonemeL, Pie.phonemeR
            , Pie.phonemeM, Pie.phonemeN
            ]
      wordInitially
      choice (fmap single laryngeals)
      consonant <- choice (fmap single consonants)
      return [consonant]

    hLossAfterV :: DoubleTapeParser Tape
    hLossAfterV = do
      let vowelTable =
            [ (Pie.phonemeA, Pie.phonemeAa)
            , (Pie.phonemeAa, Pie.phonemeAa)
            , (Pie.phonemeAAcc, Pie.phonemeAaAcc)
            , (Pie.phonemeAaAcc, Pie.phonemeAaAcc)
            , (Pie.phonemeE, Pie.phonemeEe)
            , (Pie.phonemeEe, Pie.phonemeEe)
            , (Pie.phonemeEAcc, Pie.phonemeEeAcc)
            , (Pie.phonemeEeAcc, Pie.phonemeEeAcc)
            , (Pie.phonemeI, Pie.phonemeIi)
            , (Pie.phonemeIi, Pie.phonemeIi)
            , (Pie.phonemeIAcc, Pie.phonemeIiAcc)
            , (Pie.phonemeIiAcc, Pie.phonemeIiAcc)
            , (Pie.phonemeO, Pie.phonemeOo)
            , (Pie.phonemeOo, Pie.phonemeOo)
            , (Pie.phonemeOAcc, Pie.phonemeOoAcc)
            , (Pie.phonemeOoAcc, Pie.phonemeOoAcc)
            , (Pie.phonemeU, Pie.phonemeUu)
            , (Pie.phonemeUu, Pie.phonemeUu)
            , (Pie.phonemeUAcc, Pie.phonemeUuAcc)
            , (Pie.phonemeUuAcc, Pie.phonemeUuAcc)
            ]
          makeVowelParser (inp, outp) = fmap (const outp) (single inp)
          laryngeals = [Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3] 
      newVowel <- choice (fmap makeVowelParser vowelTable)
      choice (fmap single laryngeals)
      return [newVowel]

    hVocaliz :: DoubleTapeParser Tape
    hVocaliz = do
      let laryngeals = [Pie.phonemeH1, Pie.phonemeH2, Pie.phonemeH3] 
      choice (fmap single laryngeals)
      return ["ə"]

    labiovelarMerge :: DoubleTapeParser Tape
    labiovelarMerge = do
      let velarTable =
            [ (Pie.phonemeK, Pie.phonemeKw)
            , (Pie.phonemeG, Pie.phonemeGw)
            , (Pie.phonemeGh, Pie.phonemeGwh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
      labiovelar <- choice (fmap makeVelarParser velarTable)
      single Pie.phonemeW
      return [labiovelar]

    delabBeforeU:: DoubleTapeParser Tape
    delabBeforeU = do
      let velarTable =
            [ (Pie.phonemeKw, Pie.phonemeK)
            , (Pie.phonemeGw, Pie.phonemeG)
            , (Pie.phonemeGwh, Pie.phonemeGh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
          vowels =
            [ Pie.phonemeU
            , Pie.phonemeUAcc
            , Pie.phonemeUu
            , Pie.phonemeUuAcc
            ]
      velar <- choice (fmap makeVelarParser velarTable)
      vowel <- choice (fmap single vowels)
      return [velar, vowel]

    delabAfterU:: DoubleTapeParser Tape
    delabAfterU = do
      let velarTable =
            [ (Pie.phonemeKw, Pie.phonemeK)
            , (Pie.phonemeGw, Pie.phonemeG)
            , (Pie.phonemeGwh, Pie.phonemeGh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
          vowels =
            [ Pie.phonemeU
            , Pie.phonemeUAcc
            , Pie.phonemeUu
            , Pie.phonemeUuAcc
            ]
      vowel <- choice (fmap single vowels)
      nasal <- fmap maybeToList (optional (single Pie.phonemeN))
      velar <- choice (fmap makeVelarParser velarTable)
      return (vowel : nasal ++ [velar])

    delabBeforeT :: DoubleTapeParser Tape
    delabBeforeT = do
      let velarTable =
            [ (Pie.phonemeKw, Pie.phonemeK)
            , (Pie.phonemeGw, Pie.phonemeG)
            , (Pie.phonemeGwh, Pie.phonemeGh)
            ]
          makeVelarParser (inp, outp) = fmap (const outp) (single inp)
      velar <- choice (fmap makeVelarParser velarTable)
      plosive <- single Pie.phonemeT
      return [velar, plosive]
    
    finalShortAccShift :: DoubleTapeParser Tape
    finalShortAccShift = do
      let vowels =
            [ Pie.phonemeA, Pie.phonemeAa, Pie.phonemeAAcc, Pie.phonemeAaAcc
            , Pie.phonemeE, Pie.phonemeEe, Pie.phonemeEAcc, Pie.phonemeEeAcc
            , Pie.phonemeI, Pie.phonemeIi, Pie.phonemeIAcc, Pie.phonemeIiAcc
            , Pie.phonemeO, Pie.phonemeOo, Pie.phonemeOAcc, Pie.phonemeOoAcc
            , Pie.phonemeU, Pie.phonemeUu, Pie.phonemeUAcc, Pie.phonemeUuAcc
            ]
          nonFinalTable = 
            [ (Pie.phonemeA, Pie.phonemeAAcc)
            , (Pie.phonemeAa, Pie.phonemeAaAcc)
            , (Pie.phonemeE, Pie.phonemeEAcc)
            , (Pie.phonemeEe, Pie.phonemeEeAcc)
            , (Pie.phonemeI, Pie.phonemeIAcc)
            , (Pie.phonemeIi, Pie.phonemeIiAcc)
            , (Pie.phonemeO, Pie.phonemeOAcc)
            , (Pie.phonemeOo, Pie.phonemeOoAcc)
            , (Pie.phonemeU, Pie.phonemeUAcc)
            , (Pie.phonemeUu, Pie.phonemeUuAcc)
            ]
          finalTable =
            [ (Pie.phonemeAAcc, Pie.phonemeA)
            , (Pie.phonemeEAcc, Pie.phonemeE)
            , (Pie.phonemeOAcc, Pie.phonemeO)
            ]
          makeVowelParser (inp, outp) = fmap (const outp) (single inp)
          finalParser = fmap makeVowelParser finalTable
      prevVowel <- choice (fmap makeVowelParser nonFinalTable)
      (between, final) <- manyTill_ (noneOf vowels) (choice finalParser)
      end <- chunk [Pie.punctSpace] <|> fmap (const []) eof 
      return (prevVowel : between ++ final : end)

    finalShortLoss :: DoubleTapeParser Tape
    finalShortLoss = do
      let glide = [Pie.phonemeW, Pie.phonemeY]
          finalShort = [Pie.phonemeA , Pie.phonemeE , Pie.phonemeO]
      optional (oneOf glide)
      oneOf finalShort
      chunk [Pie.punctSpace] <|> fmap (const []) eof 

    preSpirantLaw :: DoubleTapeParser Tape
    preSpirantLaw = do
      let plosivesTable =
            [ (Pie.phonemeB, Pie.phonemeP)
            , (Pie.phonemeBh, Pie.phonemeP)
            , (Pie.phonemeG, Pie.phonemeK)
            , (Pie.phonemeGh, Pie.phonemeK)
            ]
          makePlosiveParser (inp, outp) = fmap (const outp) (single inp)
      newPlosive <- choice (fmap makePlosiveParser plosivesTable)
      following <- single Pie.phonemeT
      return [newPlosive, following]

    grimmsLawVless :: DoubleTapeParser Tape
    grimmsLawVless = do
      let lenitingTable =
            [ (Pie.phonemeP, phonemeF)
            , (Pie.phonemeT, phonemeTh)
            , (Pie.phonemeK, phonemeH)
            , (Pie.phonemeKw, phonemeHw)
            , (Pie.phonemeS, phonemeS)
            ]
          nonLenitingTable =
            [ (Pie.phonemeP, phonemeP)
            , (Pie.phonemeT, phonemeT)
            , (Pie.phonemeK, phonemeH)
            , (Pie.phonemeKw, phonemeKw)
            , (Pie.phonemeS, phonemeS)
            ]
          makePlosiveParser (inp, outp) = fmap (const outp) (single inp)
      first <- choice (fmap makePlosiveParser lenitingTable)
      second <- optional (choice (fmap makePlosiveParser nonLenitingTable))
      return (first : maybeToList second)

    grimmsLawVoiced :: DoubleTapeParser Tape
    grimmsLawVoiced =
      let plosiveTable =
            [ (Pie.phonemeB, phonemeP)
            , (Pie.phonemeD, phonemeT)
            , (Pie.phonemeG, phonemeK)
            , (Pie.phonemeGw, phonemeKw)
            , (Pie.phonemeBh, phonemeB)
            , (Pie.phonemeDh, phonemeD)
            , (Pie.phonemeGh, phonemeG)
            , (Pie.phonemeGwh, phonemeGw)
            ]
          makePlosiveParser (inp, outp) = fmap (const [outp]) (single inp)
      in choice (fmap makePlosiveParser plosiveTable)

    vernersLaw :: DoubleTapeParser Tape
    vernersLaw = do
      let unaccented =
            [ Pie.phonemeA, Pie.phonemeAa, "â"
            , Pie.phonemeE, Pie.phonemeEe, phonemeEe
            , Pie.phonemeI, Pie.phonemeIi, "î"
            , Pie.phonemeO, Pie.phonemeOo, phonemeOo
            , Pie.phonemeU, Pie.phonemeUu, "û"
            ]
          accented =
            [ Pie.phonemeAAcc, Pie.phonemeAaAcc, "ấ"
            , Pie.phonemeEAcc, Pie.phonemeEeAcc, "ế"
            , Pie.phonemeIAcc, Pie.phonemeIiAcc, "î́"
            , Pie.phonemeOAcc, Pie.phonemeOoAcc, "ố"
            , Pie.phonemeUAcc, Pie.phonemeUuAcc, "û́"
            ]
          voicingTable =
            [ (phonemeF, phonemeB)
            , (phonemeTh, phonemeD)
            , (phonemeS, phonemeZ)
            , (phonemeH, phonemeG)
            , (phonemeHw, phonemeGw)
            ]
          parseUnaccented = fmap (\ph -> (False, ph)) (oneOf unaccented)
          parseAccented = fmap (\ph -> (True, ph)) (oneOf accented)
          parseNucleus = parseUnaccented <|> parseAccented
          parseVoicing (inp, outp) = fmap (const outp) (single inp)
          parseShift :: DoubleTapeParser Symbol
          parseShift = choice (fmap parseVoicing voicingTable)
      (previous, (accented, vowel)) <- manyTill_ anySingle parseNucleus
      let parseConson
            | accented = anySingle
            | not accented = parseShift <|> anySingle
          nextNucleus = fmap (const ()) (lookAhead parseNucleus)
      cluster <- manyTill parseConson (nextNucleus <|> eof)
      return (previous ++ vowel : cluster)

    accentLoss :: DoubleTapeParser Tape
    accentLoss = 
      let unaccented =
            [ Pie.phonemeA, Pie.phonemeAa, "â"
            , Pie.phonemeE, Pie.phonemeEe, phonemeEe
            , Pie.phonemeI, Pie.phonemeIi, "î"
            , Pie.phonemeO, Pie.phonemeOo, phonemeOo
            , Pie.phonemeU, Pie.phonemeUu, "û"
            ]
          accented =
            [ Pie.phonemeAAcc, Pie.phonemeAaAcc, "ấ"
            , Pie.phonemeEAcc, Pie.phonemeEeAcc, "ế"
            , Pie.phonemeIAcc, Pie.phonemeIiAcc, "î́"
            , Pie.phonemeOAcc, Pie.phonemeOoAcc, "ố"
            , Pie.phonemeUAcc, Pie.phonemeUuAcc, "û́"
            ]
          makeVowelParser (inp, outp) = fmap (const [outp]) (single inp)
      in choice (fmap makeVowelParser (zip accented unaccented))

    initLabiovelToLabial :: DoubleTapeParser Tape
    initLabiovelToLabial = do
      initial <- fmap (const [phonemeB]) (single phonemeGw) <|> return []
      rest <- manyTill anySingle eof
      return (initial ++ rest)

    assimilateNw :: DoubleTapeParser Tape
    assimilateNw = do
      single phonemeN
      single phonemeW
      return [phonemeN, phonemeN]

    assimilateLn :: DoubleTapeParser Tape
    assimilateLn = do
      single phonemeL
      single phonemeN
      return [phonemeL, phonemeL]

    assimilateZm :: DoubleTapeParser Tape
    assimilateZm = do
      single phonemeZ
      single phonemeM
      return [phonemeM, phonemeM]

    unstressedOwo :: DoubleTapeParser Tape
    unstressedOwo = do
      let vowels =
            [ Pie.phonemeA, Pie.phonemeAa, "â"
            , Pie.phonemeE, Pie.phonemeEe, phonemeEe
            , Pie.phonemeI, Pie.phonemeIi, "î"
            , Pie.phonemeO, Pie.phonemeOo, phonemeOo
            , Pie.phonemeU, Pie.phonemeUu, "û"
            ]
          parseOwo = do
            single Pie.phonemeO
            single Pie.phonemeW
            single Pie.phonemeO
            return [Pie.phonemeOo]
      (previous, vowel) <- manyTill_ anySingle (oneOf vowels)
      


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
