module Main (main) where

import           CLI             (FailureStrategy (..), Input (..), Opts (..),
                                  parseOpts)
import qualified Data.Text       as T
import           Prelude
import           System.IO       (getContents, hReady)
import           TSHM.Compiler   (CompileConfig (CompileConfig),
                                  compileDeclaration)
import           TSHM.Parser     (parseDeclaration)
import           TSHM.Reconciler (reconcile)

main :: IO ()
main = do
  stdin' <- tryGetStdin
  let strat = if isJust stdin'
                 then AllowEmpty
                 else Normal
  opts <- parseOpts strat
  code <- stdin' `whenNothing` getOptsCode opts
  case parseDeclaration code of
    Left e  -> print e *> exitFailure
    Right ast -> putTextLn . compileDeclaration $ CompileConfig (reconcile ast) (forall opts) (readonly opts)

tryGetStdin :: IO (Maybe Text)
tryGetStdin = bool (pure Nothing) getContents' =<< hReady stdin
  where getContents' = fmap T.pack . guarded (not . null) <$> getContents

getOptsCode :: Opts -> IO Text
getOptsCode opts = case input opts of
                     Eval x     -> pure x
                     FilePath x -> readFileText . T.unpack $ x
                     Stdin      -> error "Failed to get opts code (matched Stdin)."
