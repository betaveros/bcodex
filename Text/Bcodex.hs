#!/usr/bin/env runhaskell
{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex (codex) where
-- imports {{{
import Control.Arrow (left, right)
import Control.Applicative ((<$>))
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Char (digitToInt, intToDigit, isHexDigit, isAlpha, isLetter, isSpace, chr, ord, toUpper, toLower)
import Data.Function (on)
import Data.List (unfoldr, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple (swap)
import qualified Data.Map as Map
import Text.Read (readMaybe)
-- }}}
-- Cx- data and either {{{
data CxLeft = CxBadString String | CxExtra String | CxBadInt Int deriving (Eq, Ord, Show)

showCxLeft :: CxLeft -> String
showCxLeft (CxBadString s) = "{" ++ s ++ "}"
showCxLeft (CxExtra s) = s
showCxLeft (CxBadInt n) = "[" ++ show n ++ "]"

type CxElem a = Either CxLeft a
type CxList a = [CxElem a]
-- }}}
-- higher-order operations on Either {{{
mapRights :: (Functor f) => (a -> b) -> f (Either c a) -> f (Either c b)
mapRights = fmap . right

mapLefts :: (Functor f) => (a -> b) -> f (Either a c) -> f (Either b c)
mapLefts = fmap . left

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
-- }}}
-- utilities to get strings {{{
str1 :: Char -> String
str1 c = [c]

intToDigitString :: Int -> String
intToDigitString = str1 . intToDigit

chrString :: Int -> String
chrString = str1 . chr
-- }}}
-- radix things {{{
fromBaseDigits :: (Integral a) => a -> [a] -> a
fromBaseDigits base ds = foldr f 0 $ reverse ds
    where f d ttl = ttl * base + d

asBaseDigits :: Int -> Int -> [Int]
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

tokensOf :: (a -> Bool) -> [a] -> [Either [a] [a]]
tokensOf p ls
    = mapLeftRight (p . head) $ groupBy ((==) `on` p) ls

isRadixDigit :: Int -> Char -> Bool
isRadixDigit radix ch = isHexDigit ch && (digitToInt ch < radix)

splitInto :: Int -> [a] -> [[a]]
splitInto n = takeWhile (not . null) . unfoldr (Just . splitAt n)

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

fromRadixNumbers :: Int -> String -> CxList Int
fromRadixNumbers radix
    = filterDelimiterLefts . map (either (Left . CxExtra) (Right . fromBaseDigits radix . map digitToInt)) . tokensOf (isRadixDigit radix)

toRadixNumbers :: Int -> CxList Int -> CxList String
toRadixNumbers radix
    = doubleLeftSpaces . mapRights (unwords . map (map intToDigit . asBaseDigits radix)) . groupRights
-- }}}
-- alpha {{{
mod1 :: (Integral a) => a -> a -> a
a `mod1` b = let x = a `mod` b in if x == 0 then b else x

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
-- }}}
-- base 64 {{{
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
-- }}}
-- morse {{{
morseTable :: [(Char, String)]
morseTable = [ ('a', ".-"), ('b', "-..."), ('c', "-.-."), ('d', "-.."), ('e', "."), ('f', "..-."), ('g', "--."), ('h', "...."), ('i', ".."), ('j', ".---"), ('k', "-.-"), ('l', ".-.."), ('m', "--"), ('n', "-."), ('o', "---"), ('p', ".--."), ('q', "--.-"), ('r', ".-."), ('s', "..."), ('t', "-"), ('u', "..-"), ('v', "...-"), ('w', ".--"), ('x', "-..-"), ('y', "-.--"), ('z', "--.."), ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-"), ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----."), (',', "--..--"), ('.', ".-.-.-"), ('?', "..--.."), (';', "-.-.-."), (':', "---..."), ('\'', ".----."), ('-', "-....-"), ('/', "-..-."), ('(', "-.--.-"), (')', "-.--.-"), ('_', "..--.-") ]

toMorseMap :: Map.Map Char String
toMorseMap = Map.fromList morseTable

toMorse :: Char -> CxElem String
toMorse ' ' = Left $ CxExtra " / "
toMorse c = case Map.lookup (toLower c) toMorseMap of
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
-- }}}
-- translate {{{
translate :: String -> String -> Char -> Char
translate from to = let m = Map.fromList (zip from (repeatLast to)) in \c -> fromMaybe c (Map.lookup c m)
    where repeatLast [x] = repeat x
          repeatLast (x:xs) = x : repeatLast xs
          repeatLast [] = error "translate with empty from string"
-- }}}
-- fancy string things used in parsing and in-between CxCoders {{{
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

readInt :: String -> Maybe Int
readInt = readMaybe

unpl :: String -> String
unpl s = fromMaybe s $ unpluralize s

-- }}}
-- CxCoders {{{
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

applyCxCoder :: CxCoder a -> a -> String
applyCxCoder c = case c of
    Left f ->  concatMap (either showCxLeft show) . f . (:[]) . Right
    Right f -> concatMap (either showCxLeft id)  . f . (:[]) . Right
-- }}}
-- parsing command line args (synonyms etc.) {{{
parseCharClass :: String -> Maybe (Char -> Bool)
parseCharClass s = case s of
    "space"      -> Just isSpace
    "spaces"     -> Just isSpace
    "whitespace" -> Just isSpace
    "alpha"      -> Just isAlpha
    "letter"     -> Just isLetter
    "letters"    -> Just isLetter
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

parseFilterSynonym :: String -> Maybe ((a -> Bool) -> (a -> Bool))
parseFilterSynonym s = case s of
    "filter" -> Just id
    "only"   -> Just id
    "strip"  -> Just (not .)
    "drop"   -> Just (not .)
    _        -> Nothing

parseSingleStringCoder :: [String] -> Either String (CxCoder String, [String])
parseSingleStringCoder s = left ("Could not parse string coder: " ++) $ case s of
    ((unpl -> "bit"   ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 2 1,  rs)
    ((unpl -> "nybble") : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 1, rs)
    ((unpl -> "byte"  ) : rs) -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 2, rs)
    ((unpl -> "char"  ) : rs) -> Right (wrapS2I $ map (Right . ord), rs)
    (("alpha"         ) : rs) -> Right (wrapS2I   fromAlphaStream, rs)
    ((parseBaseSynonym -> Just b) : rs) -> Right (wrapS2I $ fromRadixNumbers b, rs)
    ("base" : bstr : rs) -> case readInt bstr of
        Nothing -> Left $ "Unexpected " ++ show bstr ++ " after 'base' (expecting integer)"
        Just b -> Right (wrapS2I $ fromRadixNumbers b, rs)
    (("base64"        ) : rs) -> Right (wrapS2I   fromBase64Codex, rs)
    ((readInt -> Just n) : tokenstr : rs) -> case unpl tokenstr of
        "bit"    -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 2 n, rs)
        "nybble" -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 n, rs)
        "byte"   -> Right (wrapS2I $ filterDelimiterLefts . fromRadixStream 16 (2*n), rs)
        _ -> Left $ "Expecting 'bit[s]', 'byte[s]', or 'nybble[s]' after number " ++ show n ++ ", got " ++ show tokenstr
    ("rot13" : rs) -> Right (alphaStringCoder (+13), rs)
    ("shift" : (readInt -> Just n) : rs) -> Right (alphaStringCoder (+n), rs)
    ("morse" : rs) -> Right (wrapS2S fromMorseCodex, rs)
    ("to" : "morse" : rs) -> Right (Right toMorseCodex, rs)
    ((parseFilterSynonym -> Just f) : (parseCharClass -> Just p) : rs) -> Right (Right . mapAllStrings $ filter (f p), rs)
    ("translate" : csFrom : toKeyword : csTo : rs) -> case toKeyword of
        "to" -> Right (Right . mapAllStrings $ map (translate csFrom csTo), rs)
        _ -> Left $ "Translate syntax should be 'translate _ to _', got " ++ show toKeyword
    ((parseCaseSynonym -> Just f) : rs) -> Right (Right . mapAllStrings $ map f, rs)
    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left $ "Unexpected end"

parseSingleIntCoder :: [String] -> Either String (CxCoder Int, [String])
parseSingleIntCoder s = left ("Could not parse int coder: " ++) $ case s of
    ("to" : rs0) -> case rs0 of
        ((unpl -> "bit"   ) : rs) -> Right (Right $ toRadixStream 2, rs)
        ((unpl -> "nybble") : rs) -> Right (Right $ toRadixStream 16, rs)
        ((unpl -> "Nybble") : rs) -> Right (Right $ toUpperRadixStream 16, rs)
        ((unpl -> "byte"  ) : rs) -> Right (Right $ doubleLeftSpaces . toRadixTokens 16 2, rs)
        ((unpl -> "Byte"  ) : rs) -> Right (Right $ doubleLeftSpaces . toUpperRadixTokens 16 2, rs)
        ((unpl -> "char"  ) : rs) -> Right (Right $ singleLeftSpaces . mapRights chrString, rs)
        ((unpl -> "alpha" ) : rs) -> Right (Right $ singleLeftSpaces . toAlphaStream, rs)
        ((unpl -> "Alpha" ) : rs) -> Right (Right $ singleLeftSpaces . toUpperAlphaStream, rs)
        ((unpl -> "number") : rs) -> Right (Right $ toRadixNumbers 10, rs)
        ((parseBaseSynonym -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)
        ("base" : (readInt -> Just b) : rs) -> Right (Right $ toRadixNumbers b, rs)
        ((unpl -> "base64") : rs) -> Right (Right   toBase64Codex, rs)
        ((readInt -> Just n) : tokenstr : rs) -> case unpl tokenstr of
            "bit"    -> Right (Right $ toRadixTokens 2 n, rs)
            "nybble" -> Right (Right $ toRadixTokens 16 n, rs)
            "byte"   -> Right (Right $ toRadixTokens 16 (2*n), rs)
            _ -> Left $ "Expecting 'bit[s]', 'byte[s]', or 'nybble[s]' after 'to' number " ++ show n ++ ", got " ++ show tokenstr
        (x : _) -> Left $ "Unexpected " ++ x ++ " after 'to'"
        [] -> Left "Unexpected end after 'to'"
    ("plus"  : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ (+ n), rs)
    ("minus" : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ subtract n, rs)
    ("times" : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ (* n), rs)
    ("mod"   : (readInt -> Just n) : rs) -> Right (Left . fmap . fmap $ (`mod` n), rs)
    (x : _) -> Left $ "Unexpected " ++ show x
    [] -> Left $ "Unexpected end"

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
-- }}}
codex :: [String] -> Either String (String -> String)
codex args = applyCxCoder <$> parseStringCoder args
-- vim:set fdm=marker:
