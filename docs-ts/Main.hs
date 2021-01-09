module Main (main) where

import qualified Control.Exception    as E
import           Prelude
import           System.Environment   (getArgs)
import           TSHM.Parser          (parseDeclaration)
import           TSHM.Printer         (PrintConfig (PrintConfig), printSignature)
import           Text.Megaparsec      hiding (many, some)
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data TsDoc
  = TsDocSig String
  | TsDocMisc String
  deriving (Eq, Show)

pMdCode :: String -> Parser String
pMdCode ft = string ("```" <> ft) *> eol *> someTill anySingle (try $ eol *> string "```")

pTsDocSig :: Parser TsDoc
pTsDocSig = TsDocSig <$> (string "**Signature**" *> pMdCode' "ts" <* optional (try $ pMdCode' "hs"))
  where pMdCode' :: String -> Parser String
        pMdCode' ft = eol *> eol *> pMdCode ft

-- Pretty confident there must be a better way to write this...
pTsDoc :: Parser [TsDoc]
pTsDoc = (\(xs, y) mz -> filter (/= TsDocMisc "") $ TsDocMisc xs : pure y <> concat mz)
  <$> (try (manyTill_ anySingle pTsDocSig) <|> ((, TsDocMisc "") <$> some anySingle))
  <*> optional pTsDoc

main :: IO ()
main = argGuard =<< getArgs
  where argGuard :: [String] -> IO ()
        argGuard [] = putStrLn "No input file paths provided." *> exitFailure
        argGuard xs = mapM_ (\x -> printResult x =<< rewriteFile x) xs

data Result
  = Success
  | Failure

printResult :: String -> Result -> IO ()
printResult name Success = putStrLn $ "[/] Successfully augmented \"" <> name <> "\"."
printResult name Failure = putStrLn $ "[ ] Failed to augment \"" <> name <> "\"!"

rewriteFile :: String -> IO Result
rewriteFile name = tryOr Failure $ do
  input <- readFile name
  let parsed = parse pTsDoc name input
  case parsed of
    Left _    -> pure Failure
    Right ast -> Success <$ writeFile name (reconstruct ast)

reconstruct :: [TsDoc] -> String
reconstruct = foldMap f
  where f (TsDocMisc x) = x
        f (TsDocSig x)  =
             "**Signature**\n\n"
          <> "```ts\n"
          <> x
          <> "\n```"
          <> (either (const "") (surround "\n\n```hs\n" "\n```" . printSignature . (\y -> PrintConfig y Nothing False)) . parseDeclaration $ x)

surround :: Semigroup a => a -> a -> a -> a
surround l r x = l <> x <> r

tryOr :: a -> IO a -> IO a
tryOr x f = fromRight x <$> (E.try :: IO a -> IO (Either SomeException a)) f
