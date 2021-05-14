module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import System.IO (stderr)
import qualified Langevo.PIE as PIE
import qualified Langevo.PGmc as PGmc
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
  args <- fmap (fmap Text.pack) getArgs
  word <- case args of
    [word] -> return word
    _ -> do
      Text.IO.hPutStrLn stderr "Expected exactly one argument (PIE word)"
      exitFailure
  parsedWord <- case runParser PIE.parseWord "PIE" word of
    Right parsed -> return parsed
    Left error -> do
      Text.IO.hPutStrLn stderr (Text.pack (errorBundlePretty error))
      exitFailure 
  let converted = PGmc.fromPIE parsedWord
  Text.IO.putStrLn (Text.concat converted)
