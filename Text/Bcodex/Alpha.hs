module Text.Bcodex.Alpha (
    isVowel, isConsonant,
    mod1, alphaToInt, alphaToElem, mapUnderAlpha,
    intToAlpha, intToUpperAlpha,
    fromAlphaStream,
    fromAlphaStreamCodex,
    toAlphaStream, toUpperAlphaStream) where
import Data.Char (ord, chr, toUpper, isLetter)
import Text.Bcodex.Cx
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel _   = False

isConsonant :: Char -> Bool
isConsonant c = isLetter c && not (isVowel c)

mod1 :: (Integral a) => a -> a -> a
a `mod1` b = let x = a `mod` b in if x == 0 then b else x

alphaToInt :: Char -> Maybe Int
alphaToInt ch
    | 'a' <= ch && ch <= 'z' = Just $ ord ch - ord '`'
    | 'A' <= ch && ch <= 'Z' = Just $ ord ch - ord '@'
    | otherwise              = Nothing

alphaToElem :: Char -> CxElem Int
alphaToElem ch = maybe (Left (CxExtra [ch])) Right $ alphaToInt ch

mapUnderAlpha :: (Int -> Int) -> Char -> Char
mapUnderAlpha f ch
    | 'a' <= ch && ch <= 'z' = chr $ ord '`' + f (ord ch - ord '`') `mod1` 26
    | 'A' <= ch && ch <= 'Z' = chr $ ord '@' + f (ord ch - ord '@') `mod1` 26
    | otherwise              = ch

intToAlpha :: Int -> CxElem Char
intToAlpha n
    | 1 <= n && n <= 26 = Right $ chr (ord '`' + n)
    | otherwise         = Left . CxBadInt $ n

intToUpperAlpha :: Int -> CxElem Char
intToUpperAlpha = fmap toUpper . intToAlpha

fromAlphaStream :: String -> CxList Int
fromAlphaStream = map alphaToElem

fromAlphaStreamCodex :: CxList Char -> CxList Int
fromAlphaStreamCodex = concatExtraStrings . concatMapGroupedRights fromAlphaStream

toAlphaStream :: CxList Int -> CxList Char
toAlphaStream = bindRights intToAlpha . crunchDelimiterLefts

toUpperAlphaStream :: CxList Int -> CxList Char
toUpperAlphaStream = bindRights intToUpperAlpha . crunchDelimiterLefts
