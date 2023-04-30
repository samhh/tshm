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
import           TSHM.TypeScript (ScopeRule (..))

main :: IO ()
main = do
  stdin' <- tryGetStdin
  let strat = if isJust stdin'
                 then AllowEmpty
                 else Normal
  opts <- parseOpts strat
  let scope = if exportAll opts then KeepAll else KeepExported
  code <- stdin' `whenNothing` getOptsCode opts
  case parseDeclaration code of
    Left e  -> print e *> exitFailure
    Right ast -> putTextLn . compileDeclaration $ CompileConfig (reconcile scope ast) (forall opts) (readonly opts)

tryGetStdin :: IO (Maybe Text)
tryGetStdin = (`memptyIfFalse` getContents') =<< hReady stdin
  where getContents' = fmap T.pack . guarded (not . null) <$> getContents

getOptsCode :: Opts -> IO Text
getOptsCode opts = case input opts of
                     Eval x     -> pure x
                     FilePath x -> fmap decodeUtf8 . readFileBS . T.unpack $ x
                     Stdin      -> error "Failed to get opts code (matched Stdin)."
