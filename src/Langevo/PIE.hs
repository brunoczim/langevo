module Langevo.PIE
  {-
  ( parse
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
  , punctSpace
  , punctHyphen
  )-} where

{-
import Text.Megaparsec.Char (space1)
import Langevo.Parse (symForm, parseTape, TextParser, Tape, Symbol, mainSym)

parse :: TextParser Tape
parse = parseTape parsers
  where
    parsers :: [TextParser Tape]
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
      , parseY, parseW
      , parseH1, parseH2, parseH3, parseH
      , parseHyphen, parseSpace
      ]

    parseM :: TextParser Tape
    parseM = symForm phonemeM []
    parseN :: TextParser Tape
    parseN = symForm phonemeN []
    parseP :: TextParser Tape
    parseP = symForm phonemeP []
    parseB :: TextParser Tape
    parseB = symForm phonemeB []
    parseBh :: TextParser Tape
    parseBh = symForm phonemeBh ["bʱ"]
    parseT :: TextParser Tape
    parseT = symForm phonemeT []
    parseD :: TextParser Tape
    parseD = symForm phonemeD []
    parseDh :: TextParser Tape
    parseDh = symForm phonemeDh ["dʱ"]
    parseKj :: TextParser Tape
    parseKj = symForm phonemeKj ["ḱ"]
    parseGj :: TextParser Tape
    parseGj = symForm phonemeGj ["ǵ", "ǵʰ", "ǵʱ"]
    parseGjh :: TextParser Tape
    parseGjh = symForm phonemeGjh ["ǵʱ"]
    parseK :: TextParser Tape
    parseK = symForm phonemeK []
    parseG :: TextParser Tape
    parseG = symForm phonemeG []
    parseGh :: TextParser Tape
    parseGh = symForm phonemeGh ["gʱ"]
    parseKw :: TextParser Tape
    parseKw = symForm phonemeKw []
    parseGw :: TextParser Tape
    parseGw = symForm phonemeGw []
    parseGwh :: TextParser Tape
    parseGwh = symForm phonemeGwh ["gʷʱ", "gʷʰ", "gʱʷ"]
    parseS :: TextParser Tape
    parseS = symForm phonemeS []
    parseH1 :: TextParser Tape
    parseH1 = symForm phonemeH1 ["h1"]
    parseH2 :: TextParser Tape
    parseH2 = symForm phonemeH2 ["h2"]
    parseH3 :: TextParser Tape
    parseH3 = symForm phonemeH3 ["h3"]
    parseH :: TextParser Tape
    parseH = symForm phonemeH []
    parseR :: TextParser Tape
    parseR = symForm phonemeR []
    parseL :: TextParser Tape
    parseL = symForm phonemeL []
    parseY :: TextParser Tape
    parseY = symForm phonemeY []
    parseW :: TextParser Tape
    parseW = symForm phonemeW []
    parseA :: TextParser Tape
    parseA = symForm phonemeA []
    parseE :: TextParser Tape
    parseE = symForm phonemeE []
    parseI :: TextParser Tape
    parseI = symForm phonemeI []
    parseO :: TextParser Tape
    parseO = symForm phonemeO []
    parseU :: TextParser Tape
    parseU = symForm phonemeU []
    parseAAcc :: TextParser Tape
    parseAAcc = symForm phonemeAAcc ["á"]
    parseEAcc:: TextParser Tape
    parseEAcc = symForm phonemeEAcc ["é"]
    parseIAcc :: TextParser Tape
    parseIAcc = symForm phonemeIAcc ["í"]
    parseOAcc :: TextParser Tape
    parseOAcc = symForm phonemeOAcc ["ó"]
    parseUAcc :: TextParser Tape
    parseUAcc = symForm phonemeUAcc ["ú"]
    parseAa :: TextParser Tape
    parseAa = symForm phonemeAa ["ā"]
    parseEe :: TextParser Tape
    parseEe = symForm phonemeEe ["ē"]
    parseIi :: TextParser Tape
    parseIi = symForm phonemeIi ["ī"]
    parseOo :: TextParser Tape
    parseOo = symForm phonemeOo ["ō"]
    parseUu :: TextParser Tape
    parseUu = symForm phonemeUu ["ū"]
    parseAaAcc :: TextParser Tape
    parseAaAcc = symForm phonemeAaAcc ["ā́", "ā́"]
    parseEeAcc :: TextParser Tape
    parseEeAcc = symForm phonemeEeAcc ["ḗ", "ḗ"]
    parseIiAcc :: TextParser Tape
    parseIiAcc = symForm phonemeIiAcc ["ī́", "ī́"]
    parseOoAcc :: TextParser Tape
    parseOoAcc = symForm phonemeOoAcc ["ṓ", "ṓ"]
    parseUuAcc :: TextParser Tape
    parseUuAcc = symForm phonemeUuAcc ["ū́", "ū́"]
    parseRSyl :: TextParser Tape
    parseRSyl = symForm phonemeRSyl []
    parseLSyl :: TextParser Tape
    parseLSyl = symForm phonemeLSyl []
    parseMSyl :: TextParser Tape
    parseMSyl = symForm phonemeMSyl []
    parseNSyl :: TextParser Tape
    parseNSyl = symForm phonemeNSyl []
    parseRSylAcc :: TextParser Tape
    parseRSylAcc = symForm phonemeRSylAcc ["ŕ̥", "ŕ̥"]
    parseLSylAcc :: TextParser Tape
    parseLSylAcc = symForm phonemeLSylAcc ["ĺ̥", "ĺ̥"]
    parseMSylAcc :: TextParser Tape
    parseMSylAcc = symForm phonemeMSylAcc ["ḿ̥", "ḿ̥"]
    parseNSylAcc :: TextParser Tape
    parseNSylAcc = symForm phonemeNSylAcc ["ń̥", "ń̥"]
    parseHyphen :: TextParser Tape
    parseHyphen = symForm punctHyphen []
    parseSpace :: TextParser Tape
    parseSpace = mainSym punctSpace space1 

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
-}
