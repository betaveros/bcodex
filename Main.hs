module Main (main) where

import System.Environment
import Text.Bcodex

main :: IO ()
main = do
    args <- getArgs
    case codex args of
        Left em -> error em
        Right f -> interact $ unlines . map f . lines
