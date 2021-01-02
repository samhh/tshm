module Main (main) where

import           CLI          (Input (..), parse)
import           Prelude
import           TSHM.Parser  (ParseOutput, parseSignature)
import           TSHM.Printer (PrintConfig (PrintConfig), printSignature)

main :: IO ()
main = do
  input <- parse
  render (forall input) (parseSignature (declaration input))
  where render :: Maybe String -> ParseOutput -> IO ()
        render _ (Left e)   = print e *> exitFailure
        render fa (Right x) = putStrLn . printSignature $ PrintConfig x fa

