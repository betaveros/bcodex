#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex (CxLeft(..), CxElem, CxList, CxCoder, applyCxCoder, parseStringCoder, parseIntCoder, codex) where
-- imports {{{
import Control.Arrow (left)
import Control.Applicative ((<$>))
import Data.Char (isAlpha, isLetter, isSpace, ord, toUpper, toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Text.Bcodex.Cx
import Text.Bcodex.Alpha
import Text.Bcodex.Radix
import Text.Bcodex.Morse
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils
import Text.Bcodex.Base64
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
unpl s = fromMaybe s $ unpluralize s
-- }}}
-- CxCoders {{{
type CxCoder a = Either (CxList a -> CxList Int) (CxList a -> CxList String)

unpluralize :: String -> Maybe String
unpluralize s = case last s of
    's' -> Just $ init s
    _   -> Nothing

alphaStringCoder :: (Int -> Int) -> CxCoder String
alphaStringCoder = Right . fmap . fmap . fmap . mapUnderAlpha

rcompose :: (CxList a -> CxList b) -> CxCoder b -> CxCoder a
rcompose f (Left f') = Left (f' . f)
rcompose f (Right f') = Right (f' . f)

applyCxCoder :: CxCoder a -> CxList a -> Either (CxList Int) (CxList String)
applyCxCoder = either (Left .) (Right .)

applyCxCoderToString :: CxCoder a -> a -> String
applyCxCoderToString c = case c of
    Left f ->  concatMap (either showCxLeft show) . f . (:[]) . Right
    Right f -> concatMap (either showCxLeft id)  . f . (:[]) . Right
-- }}}
-- parsing command line args (synonyms etc.) {{{
parseRadixTokenSynonym :: String -> Either String (Int, Int)
parseRadixTokenSynonym s = case unpl s of
    "bit"    -> Right (2, 1)
    "nybble" -> Right (16, 1)
    "byte"   -> Right (16, 2)
    _        -> Left "Expecting 'bit[s]', 'byte[s]', or 'nybble[s]'"

parseCharClass :: String -> Maybe (Char -> Bool)
parseCharClass s = case s of
    "space"      -> Just isSpace
    "spaces"     -> Just isSpace
    "whitespace" -> Just isSpace
    "alpha"      -> Just isAlpha
    "letter"     -> Just isLetter
    "letters"    -> Just isLetter
    "vowel"      -> Just isVowel
    "vowels"     -> Just isVowel
    "consonant"  -> Just isConsonant
    "consonants" -> Just isConsonant
    _ -> Nothing

parseBaseSynonym :: String -> Maybe Int
parseBaseSynonym s = case s of
    "number"  -> Just 10
    "numbers" -> Just 10
    "decimal" -> Just 10
    "bin"     -> Just 2
    "binary"  -> Just 2
    "oct"     -> Just 8
    "octal"   -> Just 8
    "hex"         -> Just 16
    "hexadecimal" -> Just 16
    _ -> Nothing

parseCaseSynonym :: String -> Maybe (Char -> Char)
parseCaseSynonym s = case s of
    "upper"      -> Just toUpper
    "uppercase"  -> Just toUpper
    "uppercased" -> Just toUpper
    "lower"      -> Just toLower
    "lowercase"  -> Just toLower
    "lowercased" -> Just toLower
    _ -> Nothing

parseFilterSynonym :: String -> Maybe ((a -> Bool) -> a -> Bool)
parseFilterSynonym s = case s of
    "filter" -> Just id
    "only"   -> Just id
    "strip"  -> Just (not .)
    "drop"   -> Just (not .)
    _        -> Nothing


expectNumberMeaningAfter :: String -> String -> String -> Either String Int
expectNumberMeaningAfter m s t = case readInt t of
    Just n -> Right n
    Nothing -> Left $ "Expecting number (" ++ m ++ ") after '" ++ s ++ "', got unexpected " ++ show t

parseSingleStringCoder :: [String] -> Either String (CxCoder String, [String])
parseSingleStringCoder s = left ("Could not parse string coder: " ++) $ case s of
    ((parseRadixTokenSynonym -> Right (r, a)) : rs) -> Right (Left $ concatMapRights (fromRadixStream r a) . concatRights, rs)
    ((readInt -> Just n) : tokenstr : rs) -> case parseRadixTokenSynonym tokenstr of
        Right (r, a) -> Right (Left . concatMapRights $ fromRadixStream r (a*n), rs)
        Left e -> Left $ e ++ " after number " ++ show n ++ ", got " ++ show tokenstr

    ((parseBaseSynonym -> Just b) : rs) -> Right (Left $ fromRadixNumbersCodex b, rs)
    ("base" : bstr : rs) -> do
        b <- expectNumberMeaningAfter "radix" "base" bstr
        Right (Left $ fromRadixNumbersCodex b, rs)

    ((unpl -> "char") : rs) -> Right (Left . concatMapRights $ map (Right . ord), rs)
    ("base64" : rs) -> Right (Left . concatMapRights $ fromBase64Codex, rs)

    ("alpha" : rs) -> Right (Left fromAlphaStreamCodex, rs)
    ("rot13" : rs)  -> Right (alphaStringCoder (+13), rs)
    ("atbash" : rs) -> Right (alphaStringCoder (27-), rs)
    ("shift" : a : rs) -> do
        n <- expectNumberMeaningAfter "shift amount" "shift" a
        return (alphaStringCoder (+n), rs)

    (       "morse" : rs) -> Right (Right fromMorseCodex, rs)
    ("to" : "morse" : rs) -> Right (Right toMorseCodex, rs)

    ((parseFilterSynonym -> Just f) : (parseCharClass -> Just p) : rs) -> Right (Right . mapAllStrings $ filter (f p), rs)
    ("translate" : csFrom : toKeyword : csTo : rs) -> case toKeyword of
        "to" -> Right (Right . mapAllStrings $ map (translate csFrom csTo), rs)
        _ -> Left $ "Translate syntax should be 'translate _ to _', got " ++ show toKeyword
    ("split-lines" : rs) ->
        Right (Right . mapAllStrings $ map (\x -> case x of
            ' ' -> '\n'
            _   -> x), rs)
    ((parseCaseSynonym -> Just f) : rs) -> Right (Right . mapAllStrings $ map f, rs)

    ("raw" : rs) -> Right (Right ((:[]) . Right . show), rs)

    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left "Unexpected end"

parseArithmeticOperation :: String -> Maybe (Int -> Int -> Int)
parseArithmeticOperation s = case s of
    "plus"     -> Just (+)
    "add"      -> Just (+)
    "+"        -> Just (+)
    "minus"    -> Just subtract
    "-"        -> Just subtract
    "subtract" -> Just subtract
    "times"    -> Just (*)
    "multiply" -> Just (*)
    "*"        -> Just (*)
    "x"        -> Just (*)
    "mod"      -> Just (flip mod)
    "mod1"     -> Just (flip mod1)
    _ -> Nothing

parseSingleIntCoder :: [String] -> Either String (CxCoder Int, [String])
parseSingleIntCoder s = left ("Could not parse int coder: " ++) $ case s of
    ("to" : rs0) -> case rs0 of
        ((unpl -> "bit"   ) : rs) -> Right (Right $ toRadixStream 2, rs)
        ((unpl -> "nybble") : rs) -> Right (Right $ toRadixStream 16, rs)
        ((unpl -> "Nybble") : rs) -> Right (Right $ toUpperRadixStream 16, rs)
        ((unpl -> "byte"  ) : rs) -> Right (Right $ expandExtraSpaces . toRadixTokens 16 2, rs)
        ((unpl -> "Byte"  ) : rs) -> Right (Right $ expandExtraSpaces . toUpperRadixTokens 16 2, rs)

        ((unpl -> "number") : rs) -> Right (Right $ toRadixNumbers 10, rs)
        ((parseBaseSynonym -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)
        ("base" : (readInt -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)

        ((unpl -> "char"  ) : rs) -> Right (Right $ mapRights chrString . crunchDelimiterLefts, rs)

        ((unpl -> "alpha" ) : rs) -> Right (Right   toAlphaStream, rs)
        ((unpl -> "Alpha" ) : rs) -> Right (Right   toUpperAlphaStream, rs)

        ((unpl -> "base64") : rs) -> Right (Right   toBase64Codex, rs)
        ((readInt -> Just n) : tokenstr : rs) -> case parseRadixTokenSynonym tokenstr of
            Right (r, a) -> Right (Right $ toRadixTokens r (a*n), rs)
            Left e -> Left $ e ++ " after 'to' number " ++ show n ++ ", got " ++ show tokenstr
        (x : _) -> Left $ "Unexpected " ++ x ++ " after 'to'"
        [] -> Left "Unexpected end after 'to'"

    (ostr@(parseArithmeticOperation -> Just o) : nstr : rs) -> do
        n <- expectNumberMeaningAfter "operand" ostr nstr
        Right (Left . fmap . fmap $ o n, rs)
    ("negate"  : rs) -> Right (Left . fmap $ fmap negate, rs)
    ("negated" : rs) -> Right (Left . fmap $ fmap negate, rs)

    ("raw" : rs) -> Right (Right ((:[]) . Right . show), rs)

    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left "Unexpected end"

parseCoderAfter :: CxCoder a -> [String] -> Either String (CxCoder a)
parseCoderAfter c rs = case c of
    Left  f -> rcompose f <$> parseIntCoder rs
    Right f -> rcompose f <$> parseStringCoder rs

parseStringCoder :: [String] -> Either String (CxCoder String)
parseStringCoder [] = Right (Right id)
parseStringCoder s = parseSingleStringCoder s >>= uncurry parseCoderAfter

parseIntCoder :: [String] -> Either String (CxCoder Int)
parseIntCoder [] = Right (Left id)
parseIntCoder s = parseSingleIntCoder s >>= uncurry parseCoderAfter
-- }}}
codex :: [String] -> Either String (String -> String)
codex args = applyCxCoderToString <$> parseStringCoder args
-- vim:set fdm=marker:
