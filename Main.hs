module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdin, latin1)
import Text.Bcodex

data Opts = Opts { isLinewise :: Bool, isLatin1 :: Bool, func :: String -> String }

parseArgs :: [String] -> Either String Opts
parseArgs ("all" : rargs)    = (\x -> x { isLinewise = False}) <$> parseArgs rargs
parseArgs ("latin1" : rargs) = (\x -> x { isLatin1   = True }) <$> parseArgs rargs
parseArgs rargs              = Opts True False <$> codex rargs

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Left em -> error em
        Right o -> do
            when (isLatin1 o) $ hSetEncoding stdin latin1
            let f = func o
            interact $ if isLinewise o then unlines . map f . lines
                                       else f
