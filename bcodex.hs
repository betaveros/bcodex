#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (void)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Char (digitToInt, intToDigit, isHexDigit, isAlpha, isDigit, isLetter, isSpace, chr, ord, toUpper)
import Data.Function (on)
import Data.List (unfoldr, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple (swap)
import qualified Data.Map as Map
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Test.HUnit

-- A command-line utility for converting between "encodings".
--
-- Example usage:
-- bcodex.hs bytes to chars
-- bcodex.hs 8 bits to chars
-- bcodex.hs

data CxLeft = CxBadString String | CxExtra String | CxBadInt Int deriving (Eq, Ord, Show)

showCxLeft :: CxLeft -> String
showCxLeft (CxBadString s) = "{" ++ s ++ "}"
showCxLeft (CxExtra s) = s
showCxLeft (CxBadInt n) = "[" ++ show n ++ "]"

type CxElem a = Either CxLeft a
type CxList a = [CxElem a]

-- higher-order operations on Either
swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x

mapRights :: (Functor f) => (a -> b) -> f (Either c a) -> f (Either c b)
mapRights = fmap . fmap

mapLefts :: (Functor f) => (a -> b) -> f (Either a c) -> f (Either b c)
mapLefts f = fmap (swapEither . fmap f . swapEither)

mapBadStrings :: (Functor f) => (String -> String) -> f (CxElem c) -> f (CxElem c)
mapBadStrings f = fmap f'
    where f' (Right r) = Right r
          f' (Left (CxBadString s)) = Left (CxBadString (f s))
          f' (Left (CxExtra     s)) = Left (CxExtra     (f s))
          f' (Left (CxBadInt i)) = Left (CxBadInt i)

bindRights :: (Functor f) => (a -> Either c b) -> f (Either c a) -> f (Either c b)
bindRights = fmap . (=<<)

leftRight :: (a -> Bool) -> a -> Either a a
leftRight p x = if p x then Right x else Left x

mapLeftRight :: (Functor f) => (a -> Bool) -> f a -> f (Either a a)
mapLeftRight = fmap . leftRight

groupRights :: [Either a b] -> [Either a [b]]
groupRights [] = []
groupRights (Left c : xs) = Left c : groupRights xs
groupRights (Right r : xs) =
    case groupRights xs of
        (Right rs : gps) -> Right (r:rs) : gps
        gps -> Right [r] : gps

concatExtraStrings :: CxList a -> CxList a
concatExtraStrings [] = []
concatExtraStrings (Left (CxExtra r) : xs) =
    case concatExtraStrings xs of
        (Left (CxExtra r') : bss) -> Left (CxExtra (r ++ r')) : bss
        bss -> Left (CxExtra r) : bss
concatExtraStrings (x : xs) = x : concatExtraStrings xs

ungroupRights :: [Either a [b]] -> [Either a b]
ungroupRights = concatMap (either (\a -> [Left a]) (map Right))

isDelimiterExtra :: CxElem b -> Bool
isDelimiterExtra (Left (CxExtra "" )) = True
isDelimiterExtra (Left (CxExtra " ")) = True
isDelimiterExtra (Left (CxExtra ",")) = True
isDelimiterExtra _ = False

filterDelimiterLefts :: CxList b -> CxList b
filterDelimiterLefts = filter (not . isDelimiterExtra)

crunchMorseDelimiterLefts :: CxList b -> CxList b
crunchMorseDelimiterLefts = mapMaybe f
    where f (Left (CxExtra ""   )) = Nothing
          f (Left (CxExtra " "  )) = Nothing
          f (Left (CxExtra " / ")) = Just (Left (CxExtra " "))
          f x = Just x

mapAllStrings :: (String -> String) -> CxList String -> CxList String
mapAllStrings f = map f'
    where f' (Left  (CxBadString s)) = Left (CxBadString (f s))
          f' (Left  (CxExtra     s)) = Left (CxExtra     (f s))
          f' (Right s) = Right (f s)
          f' x = x

-- utilities to get strings

str1 :: Char -> String
str1 c = [c]

intToDigitString :: Int -> String
intToDigitString = str1 . intToDigit


chrString :: Int -> String
chrString = str1 . chr

-- stuff

mod1 :: (Integral a) => a -> a -> a
a `mod1` b = let x = a `mod` b in if x == 0 then b else x

splitInto :: Int -> [a] -> [[a]]
splitInto n = takeWhile (not . null) . unfoldr (Just . splitAt n)

-- radix things

fromBaseDigits :: (Integral a) => a -> [a] -> a
fromBaseDigits base ds = foldr f 0 $ reverse ds
    where f d ttl = ttl * base + d

asBaseDigitsSized :: Int -> Int -> Int -> CxElem [Int]
asBaseDigitsSized base size num =
    case f base size num of
        Just xs -> Right $ reverse xs
        Nothing -> Left . CxBadInt $ num
    where
        f b sz n
            | sz == 0   = if n == 0 then Just [] else Nothing
            | n < 0     = Nothing
            | otherwise = (\(q, r) -> fmap (r :) (f b (sz - 1) q)) (n `quotRem` b)

asSingleBaseDigit :: Int -> Int -> CxElem Int
asSingleBaseDigit base num
    | 0 <= num && num < base = Right num
    | otherwise = Left . CxBadInt $ num

tokensOf :: (a -> Bool) -> [a] -> [Either [a] [a]]
tokensOf p ls
    = mapLeftRight (p . head) $ groupBy ((==) `on` p) ls

isRadixDigit :: Int -> Char -> Bool
isRadixDigit radix ch = isHexDigit ch && (digitToInt ch < radix)

fromRadixToken :: Int -> Int -> String -> CxList Int
fromRadixToken radix blockSize s
    = map (\block -> if length block == blockSize
        then Right (fromBaseDigits radix $ map digitToInt block)
        else Left . CxBadString $ block
    ) $ splitInto blockSize s

fromRadixStream :: Int -> Int -> String -> CxList Int
fromRadixStream radix blockSize s
    = concatMap (either (\x -> [Left (CxExtra x)]) (fromRadixToken radix blockSize)) $ tokensOf (isRadixDigit radix) s

toRadixStream :: Int -> CxList Int -> CxList String
toRadixStream radix
    = map (fmap intToDigitString . (asSingleBaseDigit radix =<<))
toUpperRadixStream :: Int -> CxList Int -> CxList String
toUpperRadixStream radix = mapRights (map toUpper) . toRadixStream radix

toRadixToken :: Int -> Int -> Int -> CxElem String
toRadixToken radix blockSize =
    fmap (map intToDigit) . asBaseDigitsSized radix blockSize

toRadixTokens :: Int -> Int -> CxList Int -> CxList String
toRadixTokens radix blockSize
    = mapRights unwords . groupRights . bindRights (toRadixToken radix blockSize)
toUpperRadixTokens :: Int -> Int -> CxList Int -> CxList String
toUpperRadixTokens radix blockSize = mapRights (map toUpper) . toRadixTokens radix blockSize

-- alpha

alphaToInt :: Char -> CxElem Int
alphaToInt ch
    | 'a' <= ch && ch <= 'z' = Right $ ord ch - ord '`'
    | 'A' <= ch && ch <= 'Z' = Right $ ord ch - ord '@'
    | otherwise              = Left . CxExtra $ [ch]

mapUnderAlpha :: (Int -> Int) -> Char -> Char
mapUnderAlpha f ch
    | 'a' <= ch && ch <= 'z' = chr $ ord '`' + f (ord ch - ord '`') `mod1` 26
    | 'A' <= ch && ch <= 'Z' = chr $ ord '@' + f (ord ch - ord '@') `mod1` 26
    | otherwise              = ch

intToAlpha :: Int -> CxElem Char
intToAlpha n
    | 1 <= n && n <= 26 = Right $ chr (ord '`' + n)
    | otherwise         = Left . CxBadInt $ n

intToAlphaString :: Int -> CxElem String
intToAlphaString = fmap str1 . intToAlpha

intToUpperAlpha :: Int -> CxElem Char
intToUpperAlpha = fmap toUpper . intToAlpha

intToUpperAlphaString :: Int -> CxElem String
intToUpperAlphaString = fmap str1 . intToUpperAlpha

fromAlphaStream :: String -> CxList Int
fromAlphaStream = map alphaToInt

toAlphaStream :: CxList Int -> CxList String
toAlphaStream = bindRights intToAlphaString

toUpperAlphaStream :: CxList Int -> CxList String
toUpperAlphaStream = bindRights intToUpperAlphaString

-- base 64 stuff, copied from my matasano work

toBase64Char :: Int -> Char
toBase64Char x
    | 0 <= x && x < 26 = chr (ord 'A' + x)
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
toBase64 [x1,x2]       = to64Fragments 3 (flip shiftL 2 $ packChunks 8    [x2,x1]) ++ "="
toBase64 [x1]          = to64Fragments 2 (flip shiftL 4 $ packChunks 8       [x1]) ++ "=="

toNBytes :: (Bits a, Num a) => Int -> a -> [a]
toNBytes n = reverse . take n . chunkStream 8

fromBase64 :: String -> [Int]
fromBase64 "" = []
fromBase64 (c1:c2:c3:c4:cs) = bytes ++ fromBase64 cs
    where bytes
           | c3 == '=' && c4 == '=' = toNBytes 1 $ flip shiftR 4 $ packChunks 6 $ map fromBase64Char [c2,c1]
           | c4 == '=' = toNBytes 2 $ flip shiftR 2 $ packChunks 6 $ map fromBase64Char [c3,c2,c1]
           | otherwise = toNBytes 3 $                 packChunks 6 $ map fromBase64Char [c4,c3,c2,c1]
fromBase64 _ = error "base-64 has wrong number of characters"

toBase64Codex :: CxList Int -> CxList String
toBase64Codex = mapRights toBase64 . groupRights . bindRights ensureBase64
    where
        ensureBase64 x = if 0 <= x && x < 256 then Right x else Left . CxBadInt $ x

fromBase64Codex :: String -> CxList Int
fromBase64Codex = mapLefts CxBadString . ungroupRights . mapRights fromBase64 . tokensOf isBase64Char

-- morse
morseTable :: [(Char, String)]
morseTable = [ ('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"), ('n', "-."), ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--.."), ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----."), (',', "--..--"), ('.', ".-.-.-"), ('?', "..--.."), (';', "-.-.-."), (':', "---..."), ('\'', ".----.'"), ('-', "-....-"), ('/', "-..-."), ('(', "-.--.-"), (')', "-.--.-"), ('_', "..--.-") ]

toMorseMap :: Map.Map Char String
toMorseMap = Map.fromList morseTable

toMorse :: Char -> CxElem String
toMorse ' ' = Left $ CxExtra " / "
toMorse c = case Map.lookup c toMorseMap of
    Just s -> Right s
    Nothing -> Left $ CxExtra [c]

toMorseCodex :: CxList String -> CxList String
toMorseCodex = mapRights unwords . groupRights . bindRights toMorse . ungroupRights

fromMorseMap :: Map.Map String Char
fromMorseMap = Map.fromList $ map swap morseTable

fromMorse :: String -> CxElem Char
fromMorse s = case Map.lookup s fromMorseMap of
    Just c -> Right c
    Nothing -> Left $ CxBadString s

fromMorseCodex :: String -> CxList String
fromMorseCodex = mapRights (:[]) . bindRights fromMorse . crunchMorseDelimiterLefts . mapLefts CxExtra . tokensOf (`elem` ".-")

-- readers
singleSpaces :: String -> String
singleSpaces "  " = " "
singleSpaces s = s

doubleSpaces :: String -> String
doubleSpaces " " = "  "
doubleSpaces s = s

mapExtraStringGroups :: (String -> String) -> CxList a -> CxList a
mapExtraStringGroups f = mapBadStrings f . concatExtraStrings

singleLeftSpaces :: CxList a -> CxList a
singleLeftSpaces = mapExtraStringGroups singleSpaces
doubleLeftSpaces :: CxList a -> CxList a
doubleLeftSpaces = mapExtraStringGroups doubleSpaces

readEither :: String -> Either String Int
readEither s = case readMaybe s of
    Just n  -> Right n
    Nothing -> Left s

type CxCoder a = Either (CxList a -> CxList Int) (CxList a -> CxList String)

wrapS2I :: (String -> CxList Int) -> CxCoder String
wrapS2I f = Left $ \cxl -> concatMap ff cxl
    where ff (Right s) = f s
          ff (Left c) = [Left c]

wrapS2S :: (String -> CxList String) -> CxCoder String
wrapS2S f = Right $ \cxl -> concatMap ff cxl
    where ff (Right s) = f s
          ff (Left c) = [Left c]

unpluralize :: String -> Maybe String
unpluralize s = case last s of
    's' -> Just $ init s
    _   -> Nothing

alphaStringCoder :: (Int -> Int) -> CxCoder String
alphaStringCoder = Right . fmap . fmap . fmap . mapUnderAlpha

rcompose :: (CxList a -> CxList b) -> CxCoder b -> CxCoder a
rcompose f (Left f') = Left (f' . f)
rcompose f (Right f') = Right (f' . f)

readInt :: String -> Maybe Int
readInt = readMaybe

unpl :: String -> String
unpl s = fromMaybe s $ unpluralize s

parseCharClass :: String -> Maybe (Char -> Bool)
parseCharClass s = case s of
    "space"      -> Just isSpace
    "spaces"     -> Just isSpace
    "whitespace" -> Just isSpace
    "alpha"      -> Just isAlpha
    "letter"     -> Just isLetter
    "letters"    -> Just isLetter
    _ -> Nothing

translate :: String -> String -> Char -> Char
translate from to = let m = Map.fromList (zip from (repeatLast to)) in \c -> fromMaybe c (Map.lookup c m)
    where repeatLast [x] = repeat x
          repeatLast (x:xs) = x : repeatLast xs
          repeatLast [] = error "translate with empty from string"

parseSingleStringCoder :: [String] -> Either String (CxCoder String, [String])
parseSingleStringCoder s = case s of
    ((unpl -> "bit"   ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 2 1,  rs)
    ((unpl -> "nybble") : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 1, rs)
    ((unpl -> "byte"  ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 2, rs)
    ((unpl -> "char"  ) : rs) -> Right (wrapS2I $ map (Right . ord), rs)
    (("alpha"         ) : rs) -> Right (wrapS2I   fromAlphaStream, rs)
    ((unpl -> "number") : rs) -> Right (wrapS2I $ filterDelimiterLefts . mapLefts CxExtra . bindRights readEither . tokensOf isDigit, rs)
    (("base64"        ) : rs) -> Right (wrapS2I   fromBase64Codex, rs)
    ((readInt -> Just n) : (unpl -> "bit"   ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 2 n, rs)
    ((readInt -> Just n) : (unpl -> "nybble") : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 n, rs)
    ((readInt -> Just n) : (unpl -> "byte"  ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 (2*n), rs)
    ("rot13" : rs) -> Right (alphaStringCoder (+13), rs)
    ("shift" : (readInt -> Just n) : rs) -> Right (alphaStringCoder (+n), rs)
    ("morse" : rs) -> Right (wrapS2S fromMorseCodex, rs)
    ("to" : "morse" : rs) -> Right (Right toMorseCodex, rs)
    ("strip" : (parseCharClass -> Just p) : rs) -> Right (Right . mapAllStrings $ filter (not . p), rs)
    ("only"  : (parseCharClass -> Just p) : rs) -> Right (Right . mapAllStrings $ filter p, rs)
    ("translate" : csFrom : "to" : csTo : rs) -> Right (Right . mapAllStrings $ map (translate csFrom csTo), rs)
    _ -> Left "Could not parse string coder"

parseSingleIntCoder :: [String] -> Either String (CxCoder Int, [String])
parseSingleIntCoder s = case s of
    ("to" : (unpl -> "bit"   ) : rs) -> Right (Right $ toRadixStream 2, rs)
    ("to" : (unpl -> "nybble") : rs) -> Right (Right $ toRadixStream 16, rs)
    ("to" : (unpl -> "Nybble") : rs) -> Right (Right $ toUpperRadixStream 16, rs)
    ("to" : (unpl -> "byte"  ) : rs) -> Right (Right $ doubleLeftSpaces . toRadixTokens 16 2, rs)
    ("to" : (unpl -> "Byte"  ) : rs) -> Right (Right $ doubleLeftSpaces . toUpperRadixTokens 16 2, rs)
    ("to" : (unpl -> "char"  ) : rs) -> Right (Right $ singleLeftSpaces . mapRights chrString, rs)
    ("to" : (unpl -> "alpha" ) : rs) -> Right (Right $ singleLeftSpaces . toAlphaStream, rs)
    ("to" : (unpl -> "Alpha" ) : rs) -> Right (Right $ singleLeftSpaces . toUpperAlphaStream, rs)
    ("to" : (unpl -> "number") : rs) -> Right (Right $ doubleLeftSpaces . mapRights (unwords . map show) . groupRights, rs)
    ("to" : (unpl -> "base64") : rs) -> Right (Right   toBase64Codex, rs)
    ("to" : (readInt -> Just n) : (unpl -> "bit"   ) : rs) -> Right (Right $ toRadixTokens 2 n, rs)
    ("to" : (readInt -> Just n) : (unpl -> "nybble") : rs) -> Right (Right $ toRadixTokens 16 n, rs)
    ("to" : (readInt -> Just n) : (unpl -> "byte"  ) : rs) -> Right (Right $ toRadixTokens 16 (2*n), rs)
    ("plus"  : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ (+ n), rs)
    ("minus" : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ subtract n, rs)
    ("times" : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ (* n), rs)
    _ -> Left "Could not parse int coder"

parseStringCoder :: [String] -> Either String (CxCoder String)
parseStringCoder [] = Right (Right id)
parseStringCoder s = do
    (c, rs) <- parseSingleStringCoder s
    case c of
        Left f -> do
            c' <- parseIntCoder rs
            return $ rcompose f c'
        Right f -> do
            c' <- parseStringCoder rs
            return $ rcompose f c'

parseIntCoder :: [String] -> Either String (CxCoder Int)
parseIntCoder [] = Right (Left id)
parseIntCoder s = do
    (c, rs) <- parseSingleIntCoder s
    case c of
        Left f -> do
            c' <- parseIntCoder rs
            return $ rcompose f c'
        Right f -> do
            c' <- parseStringCoder rs
            return $ rcompose f c'

applyCxCoder :: CxCoder a -> a -> String
applyCxCoder c = case c of
    Left f ->  concatMap (either showCxLeft show) . f . (:[]) . Right
    Right f -> concatMap (either showCxLeft id)  . f . (:[]) . Right

codex :: [String] -> Either String (String -> String)
-- codex ["rot13"] = Right $ output . toAlphaStream . mapRights ((`mod1` 26) . (+13)) . fromAlphaStream
codex args = do
    sf <- parseStringCoder args
    return $ applyCxCoder sf

tests :: Test
tests = TestList
    [ "alpha to numbers" ~: "1 2 3 4 5 6 7 8 9 10" ~=? codexw "alpha to numbers" "abcdefghij"
    , "alpha to numbers with spaces" ~: "17 21 9 3 11  2 18 15 23 14  6 15 24" ~=? codexw "alpha to numbers" "quick brown fox"
    , "alpha to numbers with garbage" ~: "17 21 9 3 11 / 2 18 15 23 14 / 6 15 24" ~=? codexw "alpha to numbers" "quick / brown / fox"
    , "alpha to numbers with more garbage" ~: "(6 15 15  2 1 18) (2 1 26  17 21 21 24)" ~=? codexw "alpha to numbers" "(foo bar) (baz quux)"
    , "alpha to bytes with more garbage" ~: "(06 0f 0f  02 01 12) (02 01 1a  11 15 15 18)" ~=? codexw "alpha to bytes" "(foo bar) (baz quux)"
    , "numbers to alpha" ~: "abcdefghij" ~=? codexw "numbers to alpha" "1 2 3 4 5 6 7 8 9 10"
    , "numbers to Alpha" ~: "ABCDEFGHIJ" ~=? codexw "numbers to Alpha" "1 2 3 4 5 6 7 8 9 10"
    , "numbers to alpha with spaces" ~: "(foo bar) (baz quux)" ~=? codexw "numbers to alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)"
    , "numbers to alpha only alpha" ~: "foobarbazquux" ~=? codexw "numbers to alpha only alpha" "(6 15 15  2 1 18) (2 1 26  17 21 21 24)"
    , "chars to bytes" ~: "3a 2d 29" ~=? codexw "chars to bytes" ":-)"
    , "chars to Bytes" ~: "3A 2D 29" ~=? codexw "chars to Bytes" ":-)"
    , "chars to Bytes strip spaces" ~: "3A2D29" ~=? codexw "chars to Bytes strip spaces" ":-)"
    , "bytes to chars" ~: ":-)" ~=? codexw "bytes to chars" "3a 2D 29"
    , "bytes to chars with spaces" ~: ":-) :-(" ~=? codexw "bytes to chars" "3a 2D 29  3A 2d 28"
    , "bytes to chars with garbage" ~: "[:-)]" ~=? codexw "bytes to chars" "[3a 2D 29]"
    , "chars to numbers" ~: "58 45 41" ~=? codexw "chars to numbers" ":-)"
    , "numbers to chars" ~: ":-)" ~=? codexw "numbers to chars" "58 45 41"
    , "numbers to chars with spaces" ~: ":-) :-(" ~=? codexw "numbers to chars" "58 45 41  58 45 40"
    , "8 bits to bytes" ~: "3a 2d 29" ~=? codexw "8 bits to bytes" "00111010 00101101 00101001"
    , "8 bits to bytes continuous" ~: "3a 2d 29" ~=? codexw "8 bits to bytes" "001110100010110100101001"
    , "8 bits to bytes strip spaces" ~: "3a2d29" ~=? codexw "8 bits to bytes strip spaces" "001110100010110100101001"
    , "8 bits to bytes with extra spaces" ~: "3a  2d  29" ~=? codexw "8 bits to bytes" "00111010  00101101  00101001"
    , "to base 64" ~: "YW55IGNhcm5hbCBwbGVhc3VyZQ==" ~=? codexw "chars to base64" "any carnal pleasure"
    , "from base 64" ~: "any carnal pleasure" ~=? codexw "base64 to chars" "YW55IGNhcm5hbCBwbGVhc3VyZQ=="
    , "to base 64 weird" ~: "Pz4/Pj8+" ~=? codexw "chars to base64" "?>?>?>"
    , "from base 64 weird" ~: "?>?>?>" ~=? codexw "base64 to chars" "Pz4/Pj8+"
    , "shift 3" ~: "sulphur" ~=? codexw "shift 3" "primero"
    , "shift 23" ~: "xYzaBc" ~=? codexw "shift 23" "aBcdEf"
    , "rot13" ~: "nowhere AbJuReR" ~=? codexw "rot13" "abjurer NoWhErE"
    , "from morse code" ~: "foo bar abcdefghijklmnopqrstuvwxyz1234567890" ~=? codexw "morse" "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----"
    , "to morse code" ~: "..-. --- --- / -... .- .-. / .- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. .---- ..--- ...-- ....- ..... -.... --... ---.. ----. -----" ~=? codexw "to morse" "foo bar abcdefghijklmnopqrstuvwxyz1234567890"
    , "translate" ~: "foo bar 01111" ~=? codexw "translate xyz to 01" "foo bar xyzzy"
    ]
    where codexw = either error id . codex . words

main :: IO ()
main = do
    args <- getArgs
    if args == ["test"] then void (runTestTT tests) else
        case codex args of
            Left em -> error em
            Right f -> interact $ unlines . map f . lines
