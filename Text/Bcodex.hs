#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex (CxLeft(..), CxElem, CxList, CxCoder, applyCxCoder, applyCxCoderToString, parseCharCoder, parseIntCoder, codex) where
-- imports {{{
import Control.Arrow (left)
import Control.Applicative ((<$>))
import Data.Char (ord, chr)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Text.Bcodex.Cx
import Text.Bcodex.Alpha
import Text.Bcodex.Radix
import Text.Bcodex.Morse
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils
import Text.Bcodex.Base64
import Text.Bcodex.Ascii
import qualified Text.Bcodex.Parse as Parse
-- }}}
-- translate {{{
translate :: String -> String -> Char -> Char
translate from to = let m = Map.fromList (zip from (repeatLast to)) in \c -> fromMaybe c (Map.lookup c m)
    where repeatLast [x] = repeat x
          repeatLast (x:xs) = x : repeatLast xs
          repeatLast [] = error "translate with empty from string"
-- }}}
-- fancy string things used in parsing and in-between CxCoders {{{
readInt :: String -> Maybe Int
readInt = readMaybe

unpl :: String -> String
unpl = Parse.maybeUnpluralize
-- }}}
-- CxCoders {{{
type CxCoder a = Either (CxList a -> CxList Int) (CxList a -> CxList Char)

alphaStringCoder :: (Int -> Int) -> CxCoder Char
alphaStringCoder = Right . fmap . fmap . mapUnderAlpha

asciiStringCoder :: (Int -> Int) -> CxCoder Char
asciiStringCoder = Right . fmap . fmap . mapUnderAscii

rcompose :: (CxList a -> CxList b) -> CxCoder b -> CxCoder a
rcompose f (Left f') = Left (f' . f)
rcompose f (Right f') = Right (f' . f)

applyCxCoder :: CxCoder a -> CxList a -> Either (CxList Int) (CxList Char)
applyCxCoder = either (Left .) (Right .)

applyCxCoderToString :: (CxLeft -> String) -> ([Int] -> String) -> (String -> String) -> CxCoder Char -> String -> String
applyCxCoderToString showLeft showInts showChars c = case c of
        Left f  -> go showInts f
        Right f -> go showChars f
    where go showRightGroup f = concatMap (either showLeft showRightGroup) . groupRights . f . map Right
-- }}}
-- parsing command line args (synonyms etc.) {{{
expectNumberMeaningAfter :: String -> String -> String -> Either String Int
expectNumberMeaningAfter m s t = case readInt t of
    Just n -> Right n
    Nothing -> Left $ "Expecting number (" ++ m ++ ") after '" ++ s ++ "', got unexpected " ++ show t

parseSingleCharCoder :: [String] -> Either String (CxCoder Char, [String])
parseSingleCharCoder s = left ("Could not parse string coder: " ++) $ case s of
    ((Parse.radixTokenSynonym -> Right (r, a)) : rs) -> Right (Left $ concatMapGroupedRights (fromRadixStream r a), rs)
    ((readInt -> Just n) : "base" : radixstr : digitsstr : rs) -> case (readInt radixstr, digitsstr) of
        (Just radix, "digits") -> Right (Left $ concatMapGroupedRights (fromRadixStream radix n), rs)
        _ -> Left $ "Expected radix and 'digits' after number " ++ show n ++ " 'base', got " ++ show radixstr ++ " " ++ show digitsstr

    ((readInt -> Just n) : tokenstr : rs) -> case Parse.radixTokenSynonym tokenstr of
        Right (radix, mul) -> Right (Left $ concatMapGroupedRights (fromRadixStream radix (mul*n)), rs)
        Left e -> Left $ e ++ " (or 'base') after number " ++ show n ++ ", got " ++ show tokenstr

    ((Parse.baseSynonym -> Just b) : rs) -> Right (Left $ fromRadixNumbersCodex b, rs)
    ("base" : bstr : rs) -> do
        b <- expectNumberMeaningAfter "radix" "base" bstr
        Right (Left $ fromRadixNumbersCodex b, rs)

    ((unpl -> "char") : rs) -> Right (Left . bindRights $ Right . ord, rs)
    ("base64" : rs) -> Right (Left $ concatMapGroupedRights fromBase64Codex, rs)

    ("alpha" : rs) -> Right (Left fromAlphaStreamCodex, rs)
    ("rot13" : rs)  -> Right (alphaStringCoder (+13), rs)
    ("atbash" : rs) -> Right (alphaStringCoder (27-), rs)
    ("rot47" : rs)  -> Right (asciiStringCoder (+47), rs)
    ("shift" : a : rs) -> do
        n <- expectNumberMeaningAfter "shift amount" "shift" a
        return (alphaStringCoder (+n), rs)
    ("pshift" : a : rs) -> do
        n <- expectNumberMeaningAfter "shift amount" "pshift" a
        return (asciiStringCoder (+n), rs)

    (       "morse" : rs) -> Right (Right fromMorseCodex, rs)
    ("to" : "morse" : rs) -> Right (Right toMorseCodex, rs)

    (f0@(Parse.filterSynonym -> Just f) : p0 : rs) -> case Parse.filterType p0 of
        Just (Parse.CharClass cc)  -> Right (Right . concatMapAllChars $ filter (f . cc) . (:[]), rs)
        Just (Parse.CxElemType et) -> Right (Right $ filter (f . et), rs)
        Nothing -> Left $ "Expecting character class after '" ++ f0 ++ "', got " ++ p0

    ("freeze" : p0 : rs) -> case Parse.filterType p0 of
        Just (Parse.CharClass cc)  -> Right (Right $ freezeCharClass cc, rs)
        Just (Parse.CxElemType et) -> Right (Right $ freezeElemType et, rs)
        Nothing -> Left $ "Expecting character class or elem type after 'freeze', got " ++ p0
    ("unfreeze" : p0 : rs) -> case Parse.filterType p0 of
        Just (Parse.CharClass cc)  -> Right (Right $ unfreezeCharClass cc, rs)
        Just (Parse.CxElemType et) -> Right (Right $ unfreezeElemType et, rs)
        Nothing -> Left $ "Expecting character class or elem type after 'unfreeze', got " ++ p0

    ("translate" : csFrom : toKeyword : csTo : rs) -> case toKeyword of
        "to" -> Right (Right . mapAllChars $ translate csFrom csTo, rs)
        _ -> Left $ "Translate syntax should be 'translate _ to _', got " ++ show toKeyword
    ("split-lines" : rs) ->
        Right (Right . mapAllChars $ (\x -> case x of
            ' ' -> '\n'
            _   -> x), rs)
    ((Parse.charMapSynonym -> Just f) : rs) -> Right (Right . mapAllChars $ f, rs)

    ("raw" : rs) -> Right (Right (map Right . show), rs)
    ("purify" : rs) -> Right (Right (map Right . rights), rs)

    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left "Unexpected end"

parseSingleIntCoder :: [String] -> Either String (CxCoder Int, [String])
parseSingleIntCoder s = left ("Could not parse int coder: " ++) $ case s of
    ("to" : rs0) -> case rs0 of
        ((unpl -> "bit"   ) : rs) -> Right (Right $ toRadixStream 2, rs)
        ((unpl -> "nybble") : rs) -> Right (Right $ toRadixStream 16, rs)
        ((unpl -> "Nybble") : rs) -> Right (Right $ toUpperRadixStream 16, rs)
        ((unpl -> "byte"  ) : rs) -> Right (Right $ expandExtraSpaces . toRadixTokens 16 2, rs)
        ((unpl -> "Byte"  ) : rs) -> Right (Right $ expandExtraSpaces . toUpperRadixTokens 16 2, rs)

        ((unpl -> "number") : rs) -> Right (Right $ toRadixNumbers 10, rs)
        ((Parse.baseSynonym -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)
        ("base" : (readInt -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)

        ((unpl -> "char"  ) : rs) -> Right (Right $ mapRights chr . crunchDelimiterLefts, rs)
        ((unpl -> "printable"  ) : rs) -> Right (Right $ bindRights visiblePrintableChr . crunchDelimiterLefts, rs)

        ((unpl -> "alpha" ) : rs) -> Right (Right   toAlphaStream, rs)
        ((unpl -> "Alpha" ) : rs) -> Right (Right   toUpperAlphaStream, rs)

        ((unpl -> "base64") : rs) -> Right (Right   toBase64Codex, rs)
        ((readInt -> Just n) : "base" : radixstr : digitsstr : rs) -> case (readInt radixstr, digitsstr) of
            (Just radix, "digits") -> Right (Right $ toRadixTokens radix n, rs)
            _ -> Left $ "Expected radix and 'digits' after 'to' number " ++ show n ++ " 'base', got " ++ show radixstr ++ " " ++ show digitsstr

        ((readInt -> Just n) : tokenstr : rs) -> case Parse.radixTokenSynonym tokenstr of
            Right (radix, a) -> Right (Right $ toRadixTokens radix (a*n), rs)
            Left e -> Left $ e ++ " (or 'base') after 'to' number " ++ show n ++ ", got " ++ show tokenstr
        (x : _) -> Left $ "Unexpected " ++ x ++ " after 'to'"
        [] -> Left "Unexpected end after 'to'"

    (ostr@(Parse.arithmeticOperation -> Just o) : nstr : rs) -> do
        n <- expectNumberMeaningAfter "operand" ostr nstr
        Right (Left . fmap . fmap $ o n, rs)
    ("negate"  : rs) -> Right (Left . fmap $ fmap negate, rs)
    ("negated" : rs) -> Right (Left . fmap $ fmap negate, rs)

    ("raw" : rs) -> Right (Right (map Right . show), rs)
    ("purify" : rs) -> Right (Left (map Right . rights), rs)

    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left "Unexpected end"

parseCoderAfter :: CxCoder a -> [String] -> Either String (CxCoder a)
parseCoderAfter c rs = case c of
    Left  f -> rcompose f <$> parseIntCoder rs
    Right f -> rcompose f <$> parseCharCoder rs

parseCharCoder :: [String] -> Either String (CxCoder Char)
parseCharCoder [] = Right (Right id)
parseCharCoder s = parseSingleCharCoder s >>= uncurry parseCoderAfter

parseIntCoder :: [String] -> Either String (CxCoder Int)
parseIntCoder [] = Right (Left id)
parseIntCoder s = parseSingleIntCoder s >>= uncurry parseCoderAfter
-- }}}
codex :: [String] -> Either String (String -> String)
codex args = applyCxCoderToString showCxLeft (unwords . map show) id <$> parseCharCoder args
-- vim:set fdm=marker:
