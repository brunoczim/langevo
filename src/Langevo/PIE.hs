module Langevo.PIE
  ( ErrorData
  , Error
  , Parser
  , Phoneme
  , Morpheme
  , Word
  , parseWords
  , parseWord
  , parseMorpheme
  , parsePhoneme
  , phonemeM
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
  ) where

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

type ErrorData = Void
type Error = ParseErrorBundle Text ErrorData
type Parser = Parsec ErrorData Text
type Phoneme = Text
type Morpheme = [Phoneme]
type Word = [Morpheme]

parseWords :: Parser [Word]
parseWords =
  let parseWordSeq = do
        word <- parseWord
        space
        return word
  in many parseWordSeq
  
parseWord :: Parser Word
parseWord = do
  first <- parseMorpheme
  let makeNothing = fmap (const Nothing)
      parseEof = makeNothing eof
      parseNext = do
        single '-'
        parseWord
      parseSpace = makeNothing space1
  let parseEnd = parseEof <|> lookAhead parseSpace
  maybeRest <- parseEnd <|> fmap Just parseNext
  case maybeRest of
    Just rest -> return (first : rest)
    Nothing -> return [first]

parseMorpheme :: Parser Morpheme
parseMorpheme = do
  first <- parsePhoneme
  let makeNothing = fmap (const Nothing)
      parseEof = makeNothing eof
      parseHyphen = makeNothing (single '-')
      parseSpace = makeNothing space1
  let parseEnd = lookAhead (parseEof <|> parseHyphen <|> parseSpace)
  maybeRest <- parseEnd <|> fmap Just parseMorpheme
  case maybeRest of
    Just rest -> return (first : rest)
    Nothing -> return [first]

parsePhoneme :: Parser Phoneme
parsePhoneme = choice parsers
  where
    parsers :: [Parser Phoneme]
    parsers =
      [ parseRSylAcc, parseLSylAcc, parseMSylAcc, parseNSylAcc
      , parseRSyl, parseLSyl, parseMSyl, parseNSyl
      , parseR, parseL, parseM, parseN
      , parseAaAcc, parseEeAcc, parseIiAcc, parseOoAcc, parseUuAcc
      , parseAa, parseEe, parseIi, parseOo, parseUu
      , parseAAcc, parseEAcc, parseIAcc, parseOAcc, parseUAcc
      , parseA, parseE, parseI, parseO, parseU
      , parseGwh, parseGjh, parseGh, parseDh, parseBh
      , parseGw, parseGj, parseG, parseD, parseB
      , parseKw, parseKj, parseK, parseT, parseP
      , parseS
      , parseY
      , parseW
      , parseH1
      , parseH2
      , parseH3
      , parseH
      ]

    parseM :: Parser Phoneme
    parseM = parseThisPhon phonemeM []
    parseN :: Parser Phoneme
    parseN = parseThisPhon phonemeN []
    parseP :: Parser Phoneme
    parseP = parseThisPhon phonemeP []
    parseB :: Parser Phoneme
    parseB = parseThisPhon phonemeB []
    parseBh :: Parser Phoneme
    parseBh = parseThisPhon phonemeBh ["bʱ"]
    parseT :: Parser Phoneme
    parseT = parseThisPhon phonemeT []
    parseD :: Parser Phoneme
    parseD = parseThisPhon phonemeD []
    parseDh :: Parser Phoneme
    parseDh = parseThisPhon phonemeDh ["dʱ"]
    parseKj :: Parser Phoneme
    parseKj = parseThisPhon phonemeKj ["ḱ"]
    parseGj :: Parser Phoneme
    parseGj = parseThisPhon phonemeGj ["ǵ", "ǵʰ", "ǵʱ"]
    parseGjh :: Parser Phoneme
    parseGjh = parseThisPhon phonemeGjh ["ǵʱ"]
    parseK :: Parser Phoneme
    parseK = parseThisPhon phonemeK []
    parseG :: Parser Phoneme
    parseG = parseThisPhon phonemeG []
    parseGh :: Parser Phoneme
    parseGh = parseThisPhon phonemeGh ["gʱ"]
    parseKw :: Parser Phoneme
    parseKw = parseThisPhon phonemeKw []
    parseGw :: Parser Phoneme
    parseGw = parseThisPhon phonemeGw []
    parseGwh :: Parser Phoneme
    parseGwh = parseThisPhon phonemeGwh ["gʷʱ", "gʷʰ", "gʱʷ"]
    parseS :: Parser Phoneme
    parseS = parseThisPhon phonemeS []
    parseH1 :: Parser Phoneme
    parseH1 = parseThisPhon phonemeH1 ["h1"]
    parseH2 :: Parser Phoneme
    parseH2 = parseThisPhon phonemeH2 ["h2"]
    parseH3 :: Parser Phoneme
    parseH3 = parseThisPhon phonemeH3 ["h3"]
    parseH :: Parser Phoneme
    parseH = parseThisPhon phonemeH []
    parseR :: Parser Phoneme
    parseR = parseThisPhon phonemeR []
    parseL :: Parser Phoneme
    parseL = parseThisPhon phonemeL []
    parseY :: Parser Phoneme
    parseY = parseThisPhon phonemeY []
    parseW :: Parser Phoneme
    parseW = parseThisPhon phonemeW []
    parseA :: Parser Phoneme
    parseA = parseThisPhon phonemeA []
    parseE :: Parser Phoneme
    parseE = parseThisPhon phonemeE []
    parseI :: Parser Phoneme
    parseI = parseThisPhon phonemeI []
    parseO :: Parser Phoneme
    parseO = parseThisPhon phonemeO []
    parseU :: Parser Phoneme
    parseU = parseThisPhon phonemeU []
    parseAAcc :: Parser Phoneme
    parseAAcc = parseThisPhon phonemeAAcc ["á"]
    parseEAcc:: Parser Phoneme
    parseEAcc = parseThisPhon phonemeEAcc ["é"]
    parseIAcc :: Parser Phoneme
    parseIAcc = parseThisPhon phonemeIAcc ["í"]
    parseOAcc :: Parser Phoneme
    parseOAcc = parseThisPhon phonemeOAcc ["ó"]
    parseUAcc :: Parser Phoneme
    parseUAcc = parseThisPhon phonemeUAcc ["ú"]
    parseAa :: Parser Phoneme
    parseAa = parseThisPhon phonemeAa ["ā"]
    parseEe :: Parser Phoneme
    parseEe = parseThisPhon phonemeEe ["ē"]
    parseIi :: Parser Phoneme
    parseIi = parseThisPhon phonemeIi ["ī"]
    parseOo :: Parser Phoneme
    parseOo = parseThisPhon phonemeOo ["ō"]
    parseUu :: Parser Phoneme
    parseUu = parseThisPhon phonemeUu ["ū"]
    parseAaAcc :: Parser Phoneme
    parseAaAcc = parseThisPhon phonemeAaAcc ["ā́", "ā́"]
    parseEeAcc :: Parser Phoneme
    parseEeAcc = parseThisPhon phonemeEeAcc ["ḗ", "ḗ"]
    parseIiAcc :: Parser Phoneme
    parseIiAcc = parseThisPhon phonemeIiAcc ["ī́", "ī́"]
    parseOoAcc :: Parser Phoneme
    parseOoAcc = parseThisPhon phonemeOoAcc ["ṓ", "ṓ"]
    parseUuAcc :: Parser Phoneme
    parseUuAcc = parseThisPhon phonemeUuAcc ["ū́", "ū́"]
    parseRSyl :: Parser Phoneme
    parseRSyl = parseThisPhon phonemeRSyl []
    parseLSyl :: Parser Phoneme
    parseLSyl = parseThisPhon phonemeLSyl []
    parseMSyl :: Parser Phoneme
    parseMSyl = parseThisPhon phonemeMSyl []
    parseNSyl :: Parser Phoneme
    parseNSyl = parseThisPhon phonemeNSyl []
    parseRSylAcc :: Parser Phoneme
    parseRSylAcc = parseThisPhon phonemeRSylAcc ["ŕ̥", "ŕ̥"]
    parseLSylAcc :: Parser Phoneme
    parseLSylAcc = parseThisPhon phonemeLSylAcc ["ĺ̥", "ĺ̥"]
    parseMSylAcc :: Parser Phoneme
    parseMSylAcc = parseThisPhon phonemeMSylAcc ["ḿ̥", "ḿ̥"]
    parseNSylAcc :: Parser Phoneme
    parseNSylAcc = parseThisPhon phonemeNSylAcc ["ń̥", "ń̥"]
    parseThisPhon :: Phoneme -> [Phoneme] -> Parser Phoneme
    parseThisPhon correct alts =
      let choices = fmap chunk (correct : alts)
      in fmap (const correct) (choice choices)

phonemeM :: Phoneme
phonemeM = "m"
phonemeN :: Phoneme
phonemeN = "n"
phonemeP :: Phoneme
phonemeP = "p"
phonemeB :: Phoneme
phonemeB = "b"
phonemeBh :: Phoneme
phonemeBh = "bʰ"
phonemeT :: Phoneme
phonemeT = "t"
phonemeD :: Phoneme
phonemeD = "d"
phonemeDh :: Phoneme
phonemeDh = "dʰ"
phonemeKj :: Phoneme
phonemeKj = "ḱ"
phonemeGj :: Phoneme
phonemeGj = "ǵ"
phonemeGjh :: Phoneme
phonemeGjh = "ǵʰ"
phonemeK :: Phoneme
phonemeK = "k"
phonemeG :: Phoneme
phonemeG = "g"
phonemeGh :: Phoneme
phonemeGh = "gʰ"
phonemeKw :: Phoneme
phonemeKw = "kʷ"
phonemeGw :: Phoneme
phonemeGw = "gʷ"
phonemeGwh :: Phoneme
phonemeGwh = "gʷʰ"
phonemeS :: Phoneme
phonemeS = "s"
phonemeH1 :: Phoneme
phonemeH1 = "h₁"
phonemeH2 :: Phoneme
phonemeH2 = "h₂"
phonemeH3 :: Phoneme
phonemeH3 = "h₃"
phonemeH :: Phoneme
phonemeH = "H"
phonemeR :: Phoneme
phonemeR = "r"
phonemeL :: Phoneme
phonemeL = "l"
phonemeY :: Phoneme
phonemeY = "y"
phonemeW :: Phoneme
phonemeW = "w"
phonemeA :: Phoneme
phonemeA = "a"
phonemeE :: Phoneme
phonemeE = "e"
phonemeI :: Phoneme
phonemeI = "i"
phonemeO :: Phoneme
phonemeO = "o"
phonemeU :: Phoneme
phonemeU = "u"
phonemeAAcc :: Phoneme
phonemeAAcc = "á"
phonemeEAcc:: Phoneme
phonemeEAcc = "é"
phonemeIAcc :: Phoneme
phonemeIAcc = "í"
phonemeOAcc :: Phoneme
phonemeOAcc = "ó"
phonemeUAcc :: Phoneme
phonemeUAcc = "ú"
phonemeAa :: Phoneme
phonemeAa = "ā"
phonemeEe :: Phoneme
phonemeEe = "ē"
phonemeIi :: Phoneme
phonemeIi = "ī"
phonemeOo :: Phoneme
phonemeOo = "ō"
phonemeUu :: Phoneme
phonemeUu = "ū"
phonemeAaAcc :: Phoneme
phonemeAaAcc = "ā́"
phonemeEeAcc :: Phoneme
phonemeEeAcc = "ḗ"
phonemeIiAcc :: Phoneme
phonemeIiAcc = "ī́"
phonemeOoAcc :: Phoneme
phonemeOoAcc = "ṓ"
phonemeUuAcc :: Phoneme
phonemeUuAcc = "ū́"
phonemeRSyl :: Phoneme
phonemeRSyl = "r̥"
phonemeLSyl :: Phoneme
phonemeLSyl = "l̥"
phonemeMSyl :: Phoneme
phonemeMSyl = "m̥"
phonemeNSyl :: Phoneme
phonemeNSyl = "n̥"
phonemeRSylAcc :: Phoneme
phonemeRSylAcc = "ŕ̥"
phonemeLSylAcc :: Phoneme
phonemeLSylAcc = "ĺ̥"
phonemeMSylAcc :: Phoneme
phonemeMSylAcc = "ḿ̥"
phonemeNSylAcc :: Phoneme
phonemeNSylAcc = "ń̥"
