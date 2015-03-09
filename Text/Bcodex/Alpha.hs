module Text.Bcodex.Alpha (
    mod1, alphaToInt, alphaToElem, mapUnderAlpha,
    intToAlpha, intToUpperAlpha,
    fromAlphaStream,
    intToAlphaString, intToUpperAlphaString,
    fromAlphaStreamCodex,
    toAlphaStream, toUpperAlphaStream) where
import Data.Char (ord, chr, toUpper)
import Text.Bcodex.Cx
import Text.Bcodex.Utils

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

intToAlphaString :: Int -> CxElem String
intToAlphaString = fmap str1 . intToAlpha

intToUpperAlphaString :: Int -> CxElem String
intToUpperAlphaString = fmap str1 . intToUpperAlpha

fromAlphaStreamCodex :: CxList String -> CxList Int
fromAlphaStreamCodex = concatExtraStrings . concatMapRights fromAlphaStream

toAlphaStream :: CxList Int -> CxList String
toAlphaStream = bindRights intToAlphaString . crunchDelimiterLefts

toUpperAlphaStream :: CxList Int -> CxList String
toUpperAlphaStream = bindRights intToUpperAlphaString . crunchDelimiterLefts
