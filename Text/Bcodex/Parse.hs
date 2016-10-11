module Text.Bcodex.Parse (
    unpluralize, maybeUnpluralize,
    radixTokenSynonym, FilterType(..), filterType,
    baseSynonym, charMapSynonym,
    filterSynonym, arithmeticOperation) where

import Data.Char (isAlpha, isDigit, isLetter, isSpace, isPunctuation,
    isSymbol, toUpper, toLower, GeneralCategory(Space), generalCategory)
import Text.Bcodex.Alpha
import Text.Bcodex.Cx
import Text.Bcodex.Utils (isRight)
import Text.Bcodex.Unicode
import Data.Maybe (fromMaybe)

unpluralize :: String -> Maybe String
unpluralize [] = Nothing
unpluralize s = case last s of
    's' -> Just $ init s
    _   -> Nothing

maybeUnpluralize :: String -> String
maybeUnpluralize s = fromMaybe s $ unpluralize s

radixTokenSynonym :: String -> Either String (Int, Int)
radixTokenSynonym s = case maybeUnpluralize s of
    "bit"    -> Right (2, 1)
    "digit"  -> Right (10, 1)
    "nybble" -> Right (16, 1)
    "byte"   -> Right (16, 2)
    _        -> Left "Expecting 'bit[s]', 'byte[s]', 'digit[s]', or 'nybble[s]'"

data FilterType a = CharClass (Char -> Bool) | CxElemType (CxElem a -> Bool)

filterType :: String -> Maybe (FilterType a)
filterType s = case s of
        -- we don't want "space" to include newlines so that it's easy to target
        -- only (horizontal) spaces and not newlines
        "space"       -> cc $ (== Space) . generalCategory
        "spaces"      -> cc $ (== Space) . generalCategory

        "whitespace"  -> cc isSpace
        "newline"     -> cc $ (`elem` "\n\r")
        "newlines"    -> cc $ (`elem` "\n\r")
        "alpha"       -> cc isAlpha
        "letter"      -> cc isLetter
        "letters"     -> cc isLetter
        "digit"       -> cc isDigit
        "digits"      -> cc isDigit
        "number"      -> cc isDigit
        "numbers"     -> cc isDigit
        "vowel"       -> cc isVowel
        "vowels"      -> cc isVowel
        "consonant"   -> cc isConsonant
        "consonants"  -> cc isConsonant
        "punctuation" -> cc isPunctuation
        "symbol"      -> cc isSymbol
        "symbols"     -> cc isSymbol
        "*"           -> cc (const True)

        '[':(cs@(_:_)) | last cs == ']' -> cc (`elem` init cs)

        "good"    -> et isRight
        "bad"     -> et isBad
        "neutral" -> et isNeutral
        "frozen"  -> et isFrozen
        "all"     -> et (const True)
        _ -> Nothing
    where cc = Just . CharClass
          et = Just . CxElemType

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

charMapSynonym :: String -> Maybe (Char -> Char)
charMapSynonym s = case s of
    "upper"      -> Just toUpper
    "uppercase"  -> Just toUpper
    "uppercased" -> Just toUpper
    "lower"      -> Just toLower
    "lowercase"  -> Just toLower
    "lowercased" -> Just toLower
    "fullwidth"  -> Just fullwidth
    "halfwidth"  -> Just halfwidth
    "circled"    -> Just circled
    "uncircled"  -> Just uncircled
    _ -> Nothing

filterSynonym :: String -> Maybe (Bool -> Bool)
filterSynonym s = case s of
    "filter" -> Just id
    "keep"   -> Just id
    "only"   -> Just id
    "take"   -> Just id
    "strip"  -> Just not
    "drop"   -> Just not
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
