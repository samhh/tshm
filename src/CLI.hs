module CLI (parseOpts, FailureStrategy (..), Opts (..), Input (..)) where

import qualified Data.Text                          as T
import           Data.Text.IO                       (hPutStrLn)
import           Data.Version                       (showVersion)
import qualified Options.Applicative                as A
import           Options.Applicative.BashCompletion (bashCompletionParser)
import qualified Options.Applicative.Common         as AC
import qualified Options.Applicative.Internal       as AI
import qualified Options.Applicative.Types          as AT
import           Paths_tshm                         (version)
import           Prelude
import           System.Environment                 (getArgs, getProgName)
import           System.Exit                        (ExitCode (ExitSuccess))

data FailureStrategy
  = Normal
  | AllowEmpty
  deriving (Eq)

data Input
  = FilePath Text
  | Eval Text
  | Stdin

data Opts = Opts
  { input          :: Input
  , forall         :: Maybe Text
  , readonly       :: Bool
  , exportAll      :: Bool
  }

defaultOpts :: Opts
defaultOpts = Opts Stdin Nothing False False

-- This reimplements a lot of internal optparse-applicative code so that we can
-- support stdin in the form of `echo abc | tshm`. In the presence of stdin
-- input, we'll use a parser without the filepath or eval options, and will
-- hook into the library's internals to understand if any options were provided
-- at all (which will still be validated and used). Check out the `Left` case
-- of `exec` for where this divergence occurs.
parseOpts :: FailureStrategy -> IO Opts
parseOpts strat = handle . exec A.defaultPrefs (withHelp parser) =<< getArgs
  where handle (A.Success x)           = pure x
        handle (A.Failure e)           = do
          progn <- getProgName
          let (msg, exit) = A.renderFailure e progn
          case exit of
            ExitSuccess -> putStrLn msg
            _           -> hPutStrLn stderr (T.pack msg)
          exitWith exit
        handle (A.CompletionInvoked x) = do
          progn <- getProgName
          msg <- A.execCompletion x progn
          putStr msg
          exitSuccess

        exec :: A.ParserPrefs -> A.ParserInfo Opts -> [String] -> A.ParserResult Opts
        exec pprefs pinfo args =
          case AI.runP p pprefs of
            (Right (Right r), _) -> A.Success r
            (Right (Left c), _)  -> A.CompletionInvoked c
            (Left err, ctx)      -> if strat == AllowEmpty && hasNoOpts err
                                      then A.Success defaultOpts
                                      else A.Failure (A.parserFailure pprefs pinfo err ctx)
          where
            pinfo' = pinfo
              { A.infoParser = (Left <$> bashCompletionParser pinfo pprefs)
                         <|> (Right <$> A.infoParser pinfo) }
            p = AC.runParserInfo pinfo' args

            hasNoOpts :: AT.ParseError -> Bool
            hasNoOpts (AT.MissingError AT.CmdStart _) = True
            hasNoOpts _                               = False

        parser = case strat of
                    AllowEmpty -> parserWithoutCode
                    Normal     -> parserWithCode

parseFilePath :: A.Parser Input
parseFilePath = FilePath <$> A.argument A.str (A.metavar "filepath" <> A.help h)
  where h = "Read and evaluate the file at the provided path"

parseEval :: A.Parser Input
parseEval = Eval <$> A.option A.str (A.short 'e' <> A.long "eval" <> A.metavar "code" <> A.help h)
  where h = "Evaluate input code directly. Alternatively the code can be supplied directly via stdin without this option"

getParser :: A.Parser Input -> A.Parser Opts
getParser p = Opts <$>
      p
  <*> (mfilter (/= "none") <$> A.optional (A.option A.str $ A.short 'f' <> A.long "forall" <> A.metavar "string" <>
        A.help "Specify a string to be used to express universal quantification, for example \"forall\" or \"∀\". If set to \"none\" or omitted, no universal quantification will be displayed"))
  <*> A.switch (A.short 'r' <> A.long "readonly" <> A.help "Display readonly modifiers")
  <*> A.switch (A.short 'a' <> A.long "all" <> A.help "Output all declarations regardless of whether or not they're exported. Useful in tandem with --eval")

parserWithoutCode :: A.Parser Opts
parserWithoutCode = getParser (pure Stdin)

parserWithCode :: A.Parser Opts
parserWithCode = getParser (parseFilePath <|> parseEval)

withHelp :: A.Parser a -> A.ParserInfo a
withHelp p = A.info (A.helper <*> v <*> p) (A.fullDesc <> A.progDesc d)
  where d = "A parser and formatter for TypeScript declarations that outputs HM-style type signatures."
        v = A.infoOption (showVersion version) (A.short 'v' <> A.long "version" <> A.help "Output version" <> A.hidden)
