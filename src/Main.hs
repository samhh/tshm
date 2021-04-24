module Main (main) where

import           CLI          (Input (..), Opts (..), parse)
import qualified Data.Text    as T
import           Prelude
import           TSHM.Parser  (parseDeclaration)
import           TSHM.Printer (PrintConfig (PrintConfig), printDeclaration)

main :: IO ()
main = do
  opts <- parse
  code <- case input opts of
    Eval x     -> pure x
    FilePath x -> readFileText $ T.unpack x
  case parseDeclaration code of
    Left e  -> print e *> exitFailure
    Right x -> putTextLn . printDeclaration $ PrintConfig x (forall opts) (readonly opts)

