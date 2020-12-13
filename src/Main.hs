module Main (main) where

import           Prelude
import           System.Environment (getArgs)
import           TSHM.Parser        (ParseOutput, parseDeclaration)
import           TSHM.Printer       (fDeclaration)

main :: IO ()
main = render . parseDeclaration =<< argGuard =<< getArgs
  where argGuard :: [String] -> IO String
        argGuard []  = putStrLn "No input provided." *> exitFailure
        argGuard [x] = pure x
        argGuard _   = putStrLn "Too many inputs provided." *> exitFailure

        render :: ParseOutput -> IO ()
        render (Left e)  = print e *> exitFailure
        render (Right x) = putStrLn . fDeclaration $ x

