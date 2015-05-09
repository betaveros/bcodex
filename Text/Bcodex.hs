#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex (CxLeft(..), CxElem, CxList, CxCoder, applyCxCoder, parseStringCoder, parseIntCoder, codex) where
-- imports {{{
import Control.Arrow (left)
import Control.Applicative ((<$>))
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Char (isAlpha, isLetter, isSpace, chr, ord, toUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Text.Bcodex.Cx
import Text.Bcodex.Alpha
import Text.Bcodex.Radix
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils
-- }}}
-- base 64 {{{
toBase64Char :: Int -> Char
toBase64Char x
    |  0 <= x && x < 26 = chr (ord 'A' + x)
    | 26 <= x && x < 52 = chr (ord 'a' + x - 26)
    | 52 <= x && x < 62 = chr (ord '0' + x - 52)
    | x == 62 = '+'
    | x == 63 = '/'
    | otherwise = error ("Out of range for base 64: " ++ show x)

fromBase64Char :: Char -> Int
fromBase64Char c
    | 'A' <= c && c <= 'Z' = ord c - ord 'A'
    | 'a' <= c && c <= 'z' = ord c - ord 'a' + 26
    | '0' <= c && c <= '9' = ord c - ord '0' + 52
    | c == '+' = 62
    | c == '/' = 63
    | c == '-' = 62
    | c == '_' = 63
    | otherwise = error ("Invalid base64 char: " ++ show c)

isBase64Char :: Char -> Bool
isBase64Char c = or [
    'A' <= c && c <= 'Z',
    'a' <= c && c <= 'z',
    '0' <= c && c <= '9',
    c `elem` "+/-_="]

chunkStream :: (Bits a, Num a) => Int -> a -> [a]
chunkStream size = map (.&. (shiftL 1 size - 1)) . iterate (`shiftR` size)
packChunks :: (Bits a, Num a) => Int -> [a] -> a
packChunks size = foldr (.|.) 0 . zipWith (flip shiftL) [0,size..]
to64Fragments :: Int -> Int -> String
to64Fragments n = reverse . take n . map toBase64Char . chunkStream 6

toBase64 :: [Int] -> String
toBase64 [] = ""
toBase64 (x1:x2:x3:xs) = to64Fragments 4 (                packChunks 8 [x3,x2,x1]) ++ toBase64 xs
toBase64 [x1,x2]       = to64Fragments 3 (flip shiftL 2 $ packChunks 8 [   x2,x1]) ++ "="
toBase64 [x1]          = to64Fragments 2 (flip shiftL 4 $ packChunks 8 [      x1]) ++ "=="

toNBytes :: (Bits a, Num a) => Int -> a -> [a]
toNBytes n = reverse . take n . chunkStream 8

fromBase64 :: String -> [Int]
fromBase64 "" = []
fromBase64 (c1:c2:c3:c4:cs) = bytes ++ fromBase64 cs
    where bytes
           | c3 == '=' && c4 == '='
                       = toNBytes 1 $ flip shiftR 4 $ packChunks 6 $ map fromBase64Char [c2,c1]
           | c4 == '=' = toNBytes 2 $ flip shiftR 2 $ packChunks 6 $ map fromBase64Char [c3,c2,c1]
           | otherwise = toNBytes 3 $                 packChunks 6 $ map fromBase64Char [c4,c3,c2,c1]
fromBase64 _ = error "base-64 has wrong number of characters"

toBase64Codex :: CxList Int -> CxList String
toBase64Codex = mapRights toBase64 . groupRights . bindRights ensureBase64
    where ensureBase64 x = if 0 <= x && x < 256 then Right x
                                                else Left . CxBadInt $ x

fromBase64Codex :: String -> CxList Int
fromBase64Codex = mapLefts CxBadString . concatMapRights (map Right . fromBase64) . tokensOf isBase64Char
-- }}}
-- morse {{{
morseTable :: [(Char, String)]
morseTable =
    [ ('a', ".-"  ), ('b', "-..."), ('c', "-.-."), ('d', "-.." ), ('e', "."   )
    , ('f', "..-."), ('g', "--." ), ('h', "...."), ('i', ".."  ), ('j', ".---")
    , ('k', "-.-" ), ('l', ".-.."), ('m', "--"  ), ('n', "-."  ), ('o', "---" )
    , ('p', ".--."), ('q', "--.-"), ('r', ".-." ), ('s', "..." ), ('t', "-"   )
    , ('u', "..-" ), ('v', "...-"), ('w', ".--" ), ('x', "-..-"), ('y', "-.--")
    , ('z', "--..")
    , ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-")
    , ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----.")
    , (',', "--..--"), ('.', ".-.-.-"), ('?', "..--.."), (';', "-.-.-.")
    , (':', "---..."), ('\'', ".----."), ('-', "-....-"), ('/', "-..-.")
    , ('(', "-.--."), (')', "-.--.-"), ('_', "..--.-")
    ]

toMorseMap :: Map.Map Char String
toMorseMap = Map.fromList morseTable

morseSpace :: CxLeft
morseSpace = CxExtra " / "

toMorse :: Char -> CxElem String
toMorse ' ' = Left morseSpace
toMorse c = case Map.lookup (toLower c) toMorseMap of
    Just s -> Right s
    Nothing -> Left $ CxExtra [c]

toMorseCodex :: CxList String -> CxList String
toMorseCodex = intersperseDelimSpaces . mapLefts f . bindRights toMorse . ungroupRights
    where f (CxDelim " ") = morseSpace
          f x = x

fromMorseMap :: Map.Map String Char
fromMorseMap = Map.fromList $ map swap morseTable

fromMorse :: String -> CxElem Char
fromMorse s = case Map.lookup s fromMorseMap of
    Just c -> Right c
    Nothing -> Left $ CxBadString s

fromMorseCodex :: CxList String -> CxList String
fromMorseCodex = mapRights (:[]) . bindRights fromMorse . crunchMorseDelimiterLefts . concatMapRights (mapLefts CxExtra . tokensOf (`elem` ".-")) . concatRights
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
