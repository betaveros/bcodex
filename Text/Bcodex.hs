#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex (CxLeft(..), CxElem, CxList, CxCoder, applyCxCoder, applyCxCoderToString, parseCharCoder, parseIntCoder, codex) where
-- imports {{{
import Control.Arrow (left)
import Control.Applicative ((<$>))
import Data.Char (ord, chr, isLetter)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (rights)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Text.Bcodex.Cx
import Text.Bcodex.Alpha
import Text.Bcodex.Radix
import Text.Bcodex.Morse
import Text.Bcodex.Braille
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils
import Text.Bcodex.Base64
import Text.Bcodex.Ascii
import Text.Bcodex.IntExpr
import qualified Text.Bcodex.Parse as Parse
-- }}}
-- translate and swap {{{

-- Given a map from some type to itself and a key, look up the key and return
-- its value if found or the key itself if not.
passthroughMapLookup :: (Ord a) => Map.Map a a -> a -> a
passthroughMapLookup m k = fromMaybe k (Map.lookup k m)

repeatLast :: [a] -> [a]
repeatLast [x] = repeat x
repeatLast (x:xs) = x : repeatLast xs
repeatLast [] = error "repeatLast on empty list"

translate :: String -> String -> Char -> Char
translate _    [] = error "translate with empty from string"
translate from to = passthroughMapLookup $ Map.fromList (zip from (repeatLast to))

repeatLastToEqualLength :: (Eq a) => [a] -> [a] -> ([a],[a])
repeatLastToEqualLength xs ys = (take n $ repeatLast xs, take n $ repeatLast ys)
    where n = max (length xs) (length ys)

swapTranslate :: String -> String -> Char -> Char
swapTranslate from to = passthroughMapLookup $ Map.fromListWith const (zip (f1 ++ t1) (t1 ++ f1))
    where (f1, t1) = repeatLastToEqualLength from to
    -- Map.fromListWith const will take the first value for duplicate keys (it
    -- feels more often useful to me because "more important" characters get
    -- listed first??)
-- }}}
-- fancy string things used in parsing and in-between CxCoders {{{
readInt :: String -> Maybe Int
readInt = readMaybe

unpl :: String -> String
unpl = Parse.maybeUnpluralize
-- }}}
-- CxCoders {{{
type CxCoder a = Either (CxList a -> CxList Int) (CxList a -> CxList Char)

-- Right ([Either CxLeft Char] -> [Either CxLeft Char]) :: CxCoder Char

alphaStringCoder :: (Char -> Bool) -> [Int -> Int] -> CxCoder Char
alphaStringCoder p = Right . zipApplySomeRights p . map mapUnderAlpha

asciiStringCoder :: (Char -> Bool) -> [Int -> Int] -> CxCoder Char
asciiStringCoder p = Right . zipApplySomeRights p . map mapUnderAscii

shiftInStringCoder :: String -> Int -> CxCoder Char
shiftInStringCoder s delta' = Right . fmap . fmap $ passthroughMapLookup shiftMap
    where delta = delta' `mod` length s
          shiftMap = Map.fromListWith const $ zip s (drop delta s ++ take delta s)
          -- Map.fromListWith const will take the first value for duplicate
          -- keys (it feels more often useful to me, so that "shift in 011 by
          -- 1" differs from "shift in 01 by 1"; but either way it's not a big
          -- deal)

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

expectIntExprMeaningAfter :: String -> String -> String -> Either String [Int]
expectIntExprMeaningAfter m s t = case parseIntExpr t of
    Just n -> Right n
    Nothing -> Left $ "Expecting integer expression (" ++ m ++ ") after '" ++ s ++ "', got unexpected " ++ show t


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
    ("rot13" : rs)  -> Right (alphaStringCoder isLetter $ repeat (+13), rs)
    ("atbash" : rs) -> Right (alphaStringCoder isLetter $ repeat (27-), rs)
    ("rot47" : rs)  -> Right (asciiStringCoder isPrintableAscii $ repeat (+47), rs)
    ("shift" : "in" : sis : byKeyword : amtStr : rs) -> case byKeyword of
        "by" -> do
            n <- expectNumberMeaningAfter "shift amount" "shift" amtStr
            return (shiftInStringCoder sis n, rs)
        _ -> Left $ "Shift in syntax should be 'shift in _ by _', got " ++ show byKeyword
    ("shift" : amtStr : rs) -> do
        ns <- expectIntExprMeaningAfter "shift amount" "shift" amtStr
        return (alphaStringCoder isLetter $ map (+) ns, rs)
    ("pshift" : amtStr : rs) -> do
        ns <- expectIntExprMeaningAfter "shift amount" "pshift" amtStr
        return (asciiStringCoder isPrintableAscii $ map (+) ns, rs)
    ("shift!" : amtStr : rs) -> do
        ns <- expectIntExprMeaningAfter "shift amount" "shift!" amtStr
        return (alphaStringCoder (const True) $ map (+) ns, rs)
    ("pshift!" : amtStr : rs) -> do
        ns <- expectIntExprMeaningAfter "shift amount" "pshift!" amtStr
        return (asciiStringCoder (const True) $ map (+) ns, rs)

    ("vigenere" : key : rs) -> case catMaybes (map alphaToInt key) of
        [] -> error "cannot vigenere with empty key"
        ss -> return (alphaStringCoder isLetter $ cycle (map ((+) . subtract 1) ss), rs)

    ("unvigenere" : key : rs) -> case catMaybes (map alphaToInt key) of
        [] -> error "cannot vigenere with empty key"
        ss -> return (alphaStringCoder isLetter $ cycle (map (subtract . subtract 1) ss), rs)

    (       "morse" : rs) -> Right (Right fromMorseCodex, rs)
    ("to" : "morse" : rs) -> Right (Right toMorseCodex, rs)

    ((unpl -> "braille-pattern") : rs) -> Right (Left fromBraillePatternCodex, rs)
    (       "braille" : rs) -> Right (Right $ bindRights fromBraille, rs)
    ("to" : "braille" : rs) -> Right (Right $ bindRights   toBraille, rs)

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
    ("swap" : csFrom : withKeyword : csTo : rs) -> case withKeyword of
        "with" -> Right (Right . mapAllChars $ swapTranslate csFrom csTo, rs)
        _ -> Left $ "Swap syntax should be 'swap _ with _', got " ++ show withKeyword
    ("reverse-words" : rs) -> Right (Right $ cxReverseWords, rs)
    ("distribute" : nStr : rs) -> do
        n <- expectNumberMeaningAfter "number of lines" "distribute" nStr
        return (Right $ cxDistributeLinesTo n, rs)
    ("interleave" : nStr : rs) -> do
        n <- expectNumberMeaningAfter "number of lines" "interleave" nStr
        return (Right $ cxInterleaveLinesBy n, rs)

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
        ((unpl -> "braille-pattern") : rs) -> Right (Right toBraillePatternCodex, rs)
        ((readInt -> Just n) : "base" : radixstr : digitsstr : rs) -> case (readInt radixstr, digitsstr) of
            (Just radix, "digits") -> Right (Right $ toRadixTokens radix n, rs)
            _ -> Left $ "Expected radix and 'digits' after 'to' number " ++ show n ++ " 'base', got " ++ show radixstr ++ " " ++ show digitsstr

        ((readInt -> Just n) : tokenstr : rs) -> case Parse.radixTokenSynonym tokenstr of
            Right (radix, a) -> Right (Right $ toRadixTokens radix (a*n), rs)
            Left e -> Left $ e ++ " (or 'base') after 'to' number " ++ show n ++ ", got " ++ show tokenstr
        (x : _) -> Left $ "Unexpected " ++ x ++ " after 'to'"
        [] -> Left "Unexpected end after 'to'"

    (ostr@(Parse.arithmeticOperation -> Just o) : nstr : rs) -> do
        ns <- expectIntExprMeaningAfter "operand" ostr nstr
        Right (Left . zipApplySomeRights (const True) $ map o ns, rs)
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
