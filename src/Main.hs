module Main (main) where

import           CLI          (Input (..), parse)
import           Prelude
import           TSHM.Parser  (parseSignature)
import           TSHM.Printer (PrintConfig (PrintConfig), printSignature)

main :: IO ()
main = do
  input <- parse
  case parseSignature (declaration input) of
    Left e  -> print e *> exitFailure
    Right x -> putStrLn . printSignature $ PrintConfig x (forall input) (readonly input)

