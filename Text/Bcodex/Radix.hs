module Text.Bcodex.Radix (
    isGenDigit, genDigitToInt, intToGenDigit,
    fromBaseDigits, asBaseDigits, asBaseDigitsSized, asSingleBaseDigit,
    fromRadixToken, fromRadixStream, toRadixStream, toUpperRadixStream,
    toRadixToken, toRadixTokens, toUpperRadixTokens,
    fromRadixNumbers, fromRadixNumbersCodex,
    toRadixNumbers
    ) where

import Data.Char (ord, chr, toUpper)
import Text.Bcodex.Cx
import Text.Bcodex.Alpha
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils

-- gen = general = supports up to base 36
isGenDigit :: Int -> Char -> Bool
isGenDigit radix ch
    | radix <= 10 = '0' <= ch && ord ch - ord '0' < radix
    | otherwise = ('0' <= ch && ch <= '9')
                  || maybe False (< radix - 9) (alphaToInt ch)

genDigitToInt :: Char -> Int
genDigitToInt ch
    | '0' <= ch && ch <= '9' = ord ch - ord '0'
    | Just n <- alphaToInt ch = n + 9
    | otherwise = error $ "genDigitToInt failed, unexpected char " ++ show ch

intToGenDigit :: Int -> Char
intToGenDigit n
    | n <= 9 = chr (ord '0' + n)
    | otherwise = chr (ord 'a' + n - 10)

fromBaseDigits :: (Integral a) => a -> [a] -> a
fromBaseDigits base ds = foldr f 0 $ reverse ds
    where f d ttl = ttl * base + d

asBaseDigits :: Int -> Int -> [Int]
asBaseDigits _ 0 = [0]
asBaseDigits base num = reverse $ f num
    where f n
            | n == 0 = []
            | otherwise = (\(q, r) -> r : f q) (n `quotRem` base)

asBaseDigitsSized :: Int -> Int -> Int -> CxElem [Int]
asBaseDigitsSized base size num =
    case f size num of
        Just xs -> Right $ reverse xs
        Nothing -> Left . CxBadInt $ num
    where
        f sz n
            | sz == 0   = if n == 0 then Just [] else Nothing
            | n < 0     = Nothing
            | otherwise = (\(q, r) -> fmap (r :) (f (sz - 1) q)) (n `quotRem` base)

asSingleBaseDigit :: Int -> Int -> CxElem Int
asSingleBaseDigit base num
    | 0 <= num && num < base = Right num
    | otherwise = Left . CxBadInt $ num

fromRadixToken :: Int -> Int -> String -> CxList Int
fromRadixToken radix blockSize s
    = map (\block -> if length block == blockSize
        then Right (fromBaseDigits radix $ map genDigitToInt block)
        else Left . CxBadString $ block
    ) $ splitInto blockSize s

fromRadixStream :: Int -> Int -> String -> CxList Int
fromRadixStream radix blockSize
    = concatMapRights (fromRadixToken radix blockSize) . mapLefts extraOrDelim . tokensOf (isGenDigit radix)

toRadixStream :: Int -> CxList Int -> CxList String
toRadixStream radix
    = map (fmap intToGenDigitString . (asSingleBaseDigit radix =<<)) . crunchDelimiterLefts
toUpperRadixStream :: Int -> CxList Int -> CxList String
toUpperRadixStream radix = mapRights (map toUpper) . toRadixStream radix

toRadixToken :: Int -> Int -> Int -> CxElem String
toRadixToken radix blockSize =
    fmap (map intToGenDigit) . asBaseDigitsSized radix blockSize

toRadixTokens :: Int -> Int -> CxList Int -> CxList String
toRadixTokens radix blockSize
    = intersperseDelimSpaces . bindRights (toRadixToken radix blockSize)
toUpperRadixTokens :: Int -> Int -> CxList Int -> CxList String
toUpperRadixTokens radix blockSize = mapRights (map toUpper) . toRadixTokens radix blockSize

fromRadixNumbers :: Int -> String -> CxList Int
fromRadixNumbers radix
    = map (either (Left . delimOrShrink) (Right . fromBaseDigits radix . map genDigitToInt)) . tokensOf (isGenDigit radix)

fromRadixNumbersCodex :: Int -> CxList String -> CxList Int
fromRadixNumbersCodex radix = concatMapRights (fromRadixNumbers radix) . concatRights . shrinkExtraSpaces

intToGenDigitString :: Int -> String
intToGenDigitString = str1 . intToGenDigit

toRadixNumbers :: Int -> CxList Int -> CxList String
toRadixNumbers radix
    = expandExtraSpaces . intersperseDelimSpaces . mapRights (map intToGenDigit . asBaseDigits radix)
