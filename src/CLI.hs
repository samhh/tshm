module CLI (parse, Opts (..), Input (..)) where

import qualified Options.Applicative as A
import           Prelude
import Paths_tshm (version)
import Data.Version (showVersion)

data Input
  = FilePath String
  | Eval String

data Opts = Opts
  { input    :: Input
  , forall   :: Maybe String
  , readonly :: Bool
  }

parseFilePath :: A.Parser Input
parseFilePath = FilePath <$> A.argument A.str (A.metavar "filepath" <> A.help h)
  where h = "Read and evaluate the file at the provided path"

parseEval :: A.Parser Input
parseEval = Eval <$> A.option A.str (A.short 'e' <> A.long "eval" <> A.metavar "code" <> A.help h)
  where h = "Evaluate input code directly"

parser :: A.Parser Opts
parser = Opts <$>
      (parseFilePath <|> parseEval)
  <*> (mfilter (/= "none") <$> A.optional (A.option A.str $ A.short 'f' <> A.long "forall" <> A.metavar "string" <>
        A.help "Specify a string to be used to express universal quantification, for example \"forall\" or \"∀\". If set to \"none\" or omitted, no universal quantification will be displayed"))
  <*> A.switch (A.short 'r' <> A.long "readonly" <> A.help "Display readonly modifiers")

withHelp :: A.Parser a -> A.ParserInfo a
withHelp p = A.info (A.helper <*> v <*> p) (A.fullDesc <> A.progDesc d)
  where d = "A parser and formatter for TypeScript declarations that outputs HM-style type signatures."
        v = A.infoOption (showVersion version) (A.short 'v' <> A.long "version" <> A.help "Output version")

-- | Parse command-line options. The library we're using for this will handle
-- the possibility of failure for us, which isn't encoded in the type
-- signature.
parse :: IO Opts
parse = A.execParser $ withHelp parser

