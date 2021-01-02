module CLI (parse, Input(..)) where

import qualified Options.Applicative as A
import           Prelude

data Input = Input
  { forall      :: Maybe String
  , declaration :: String
  }

parser :: A.Parser Input
parser = Input <$>
      (mfilter (/= "none") <$> A.optional
        (  A.option A.str $
           A.short 'f'
        <> A.long "forall"
        <> A.metavar "string"
        <> A.help "Specify a string to be used to express universal quantification, for example \"forall\" or \"∀\". If set to \"none\" or omitted, no universal quantification will be displayed"
        ))
  <*> A.argument A.str (A.metavar "input")

help :: A.Parser a -> A.ParserInfo a
help p = A.info (A.helper <*> p) (A.fullDesc <> A.progDesc d)
  where d = "A parser and formatter for TypeScript declarations that outputs HM-style type signatures."

-- | Parse command-line options. The library we're using for this will handle
-- the possibility of failure for us, which isn't encoded in the type
-- signature.
parse :: IO Input
parse = A.execParser $ help parser

