module Text.Bcodex.Parse (
    unpluralize, maybeUnpluralize,
    radixTokenSynonym, charClass, baseSynonym, caseSynonym,
    filterSynonym, arithmeticOperation) where

import Data.Char (isAlpha, isLetter, isSpace, toUpper, toLower)
import Text.Bcodex.Alpha
import Data.Maybe (fromMaybe)

unpluralize :: String -> Maybe String
unpluralize s = case last s of
    's' -> Just $ init s
    _   -> Nothing

maybeUnpluralize :: String -> String
maybeUnpluralize s = fromMaybe s $ unpluralize s

radixTokenSynonym :: String -> Either String (Int, Int)
radixTokenSynonym s = case maybeUnpluralize s of
    "bit"    -> Right (2, 1)
    "nybble" -> Right (16, 1)
    "byte"   -> Right (16, 2)
    _        -> Left "Expecting 'bit[s]', 'byte[s]', or 'nybble[s]'"

charClass :: String -> Maybe (Char -> Bool)
charClass s = case s of
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

baseSynonym :: String -> Maybe Int
baseSynonym s = case s of
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

caseSynonym :: String -> Maybe (Char -> Char)
caseSynonym s = case s of
    "upper"      -> Just toUpper
    "uppercase"  -> Just toUpper
    "uppercased" -> Just toUpper
    "lower"      -> Just toLower
    "lowercase"  -> Just toLower
    "lowercased" -> Just toLower
    _ -> Nothing

filterSynonym :: String -> Maybe ((a -> Bool) -> a -> Bool)
filterSynonym s = case s of
    "filter" -> Just id
    "only"   -> Just id
    "strip"  -> Just (not .)
    "drop"   -> Just (not .)
    _        -> Nothing

arithmeticOperation :: String -> Maybe (Int -> Int -> Int)
arithmeticOperation s = case s of
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
