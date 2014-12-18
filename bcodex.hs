#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Arrow (second)
import Control.Monad (void)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Data.Char (digitToInt, intToDigit, isHexDigit, isDigit, chr, ord, toUpper)
import Data.Function (on)
import Data.List (unfoldr, groupBy)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Test.HUnit
import qualified Data.Map as M (Map, fromList, lookup)

-- A command-line utility for converting between "encodings".
--
-- Example usage:
-- bcodex.hs bytes to chars
-- bcodex.hs 8 bits to chars
-- bcodex.hs

type CodexList = [Either String Int]
type CodexOutput = [Either String String]

-- higher-order operations on Either
swapEither :: Either a b -> Either b a
swapEither (Left x) = Right x
swapEither (Right x) = Left x

mapRights :: (Functor f) => (a -> b) -> f (Either c a) -> f (Either c b)
mapRights = fmap . fmap

mapLefts :: (Functor f) => (a -> b) -> f (Either a c) -> f (Either b c)
mapLefts f = fmap (swapEither . fmap f . swapEither)

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

groupLefts :: [Either a b] -> [Either [a] b]
groupLefts = map swapEither . groupRights . map swapEither

ungroupRights :: [Either a [b]] -> [Either a b]
ungroupRights = concatMap (either (\a -> [Left a]) (map Right))

isDelimiterLeft :: Either String b -> Bool
isDelimiterLeft (Left "") = True
isDelimiterLeft (Left " ") = True
isDelimiterLeft (Left ",") = True
isDelimiterLeft _ = False

filterDelimiterLefts :: [Either String b] -> [Either String b]
filterDelimiterLefts = filter (not . isDelimiterLeft)

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

asBaseDigitsSized :: Int -> Int -> Int -> Either String [Int]
asBaseDigitsSized base size num =
    case f base size num of
        Just xs -> Right $ reverse xs
        Nothing -> Left $ "[" ++ show num ++ "]"
    where
        f b sz n
            | sz == 0   = if n == 0 then Just [] else Nothing
            | n < 0     = Nothing
            | otherwise = (\(q, r) -> fmap (r :) (f b (sz - 1) q)) (n `quotRem` b)

asSingleBaseDigit :: Int -> Int -> Either String Int
asSingleBaseDigit base num
    | 0 <= num && num < base = Right num
    | otherwise = Left $ "[" ++ show num ++ "]"

tokensOf :: (a -> Bool) -> [a] -> [Either [a] [a]]
tokensOf p ls
    = mapLeftRight (p . head) $ groupBy ((==) `on` p) ls

isRadixDigit :: Int -> Char -> Bool
isRadixDigit radix ch = isHexDigit ch && (digitToInt ch < radix)

fromRadixToken :: Int -> Int -> String -> CodexList
fromRadixToken radix blockSize s
    = map (\block -> if length block == blockSize
        then Right (fromBaseDigits radix $ map digitToInt block)
        else Left $ "[" ++ block ++ "]"
    ) $ splitInto blockSize s

fromRadixStream :: Int -> Int -> String -> CodexList
fromRadixStream radix blockSize s
    = concatMap (either (\x -> [Left x]) (fromRadixToken radix blockSize)) $ tokensOf (isRadixDigit radix) s

toRadixStream :: Int -> CodexList -> CodexOutput
toRadixStream radix
    = map (fmap intToDigitString . (asSingleBaseDigit radix =<<))
toUpperRadixStream :: Int -> CodexList -> CodexOutput
toUpperRadixStream radix = mapRights (map toUpper) . toRadixStream radix

toRadixToken :: Int -> Int -> Int -> Either String String
toRadixToken radix blockSize =
    fmap (map intToDigit) . asBaseDigitsSized radix blockSize

toRadixTokens :: Int -> Int -> CodexList -> CodexOutput
toRadixTokens radix blockSize
    = mapRights unwords . groupRights . bindRights (toRadixToken radix blockSize)
toUpperRadixTokens :: Int -> Int -> CodexList -> CodexOutput
toUpperRadixTokens radix blockSize = mapRights (map toUpper) . toRadixTokens radix blockSize

-- alpha

alphaToInt :: Char -> Either String Int
alphaToInt ch
    | 'a' <= ch && ch <= 'z' = Right $ ord ch - ord '`'
    | 'A' <= ch && ch <= 'Z' = Right $ ord ch - ord '@'
    | otherwise              = Left [ch]

intToAlpha :: Int -> Either String Char
intToAlpha n
    | 1 <= n && n <= 26 = Right $ chr (ord '`' + n)
    | otherwise         = Left $ "[" ++ show n ++ "]"

intToAlphaString :: Int -> Either String String
intToAlphaString = fmap str1 . intToAlpha

intToUpperAlpha :: Int -> Either String Char
intToUpperAlpha = fmap toUpper . intToAlpha

intToUpperAlphaString :: Int -> Either String String
intToUpperAlphaString = fmap str1 . intToUpperAlpha

fromAlphaStream :: String -> CodexList
fromAlphaStream = map alphaToInt

toAlphaStream :: CodexList -> CodexOutput
toAlphaStream = bindRights intToAlphaString

toUpperAlphaStream :: CodexList -> CodexOutput
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

toBase64Codex :: CodexList -> CodexOutput
toBase64Codex = mapRights toBase64 . groupRights . bindRights ensureBase64
    where
        ensureBase64 x = if 0 <= x && x < 256 then Right x else Left $ show x

fromBase64Codex :: String -> CodexList
fromBase64Codex = ungroupRights . mapRights fromBase64 . tokensOf isBase64Char

-- readers
singleSpaces :: String -> String
singleSpaces "  " = " "
singleSpaces s = s

doubleSpaces :: String -> String
doubleSpaces " " = "  "
doubleSpaces s = s

mapLeftStringGroups :: (String -> String) -> [Either String a] -> [Either String a]
mapLeftStringGroups f = mapLefts (f . concat) . groupLefts

singleLeftSpaces :: [Either String a] -> [Either String a]
singleLeftSpaces = mapLeftStringGroups singleSpaces
doubleLeftSpaces :: [Either String a] -> [Either String a]
doubleLeftSpaces = mapLeftStringGroups doubleSpaces

readEither :: String -> Either String Int
readEither s = case readMaybe s of
    Just n  -> Right n
    Nothing -> Left s

argReaderList :: M.Map String (Int -> String -> CodexList)
argReaderList = M.fromList [
    ("bit", \arg -> filterDelimiterLefts . fromRadixStream 2 arg),
    ("nybble", \arg -> filterDelimiterLefts . fromRadixStream 16 arg),
    ("byte", \arg -> filterDelimiterLefts . fromRadixStream 16 (2*arg))]
plainReaderList :: M.Map String (String -> CodexList)
plainReaderList = M.fromList [
    ("bit", filterDelimiterLefts . fromRadixStream 2 1),
    ("nybble", filterDelimiterLefts . fromRadixStream 16 1),
    ("byte", filterDelimiterLefts . fromRadixStream 16 2),
    ("char", map (Right . ord)),
    ("alpha", fromAlphaStream),
    ("number", filterDelimiterLefts . bindRights readEither . tokensOf isDigit),
    ("base64", fromBase64Codex)
    ]

argWriterList :: M.Map String (Int -> CodexList -> CodexOutput)
argWriterList = M.fromList [
    ("bit", toRadixTokens 2),
    ("nybble", toRadixTokens 16),
    ("byte", toRadixTokens 16 . (*2)) ]
plainWriterList :: M.Map String (CodexList -> CodexOutput)
plainWriterList = M.fromList [
    ("bit", toRadixStream 2),
    ("nybble", toRadixStream 16),
    ("Nybble", toUpperRadixStream 16),
    ("byte", doubleLeftSpaces . toRadixTokens 16 2),
    ("Byte", doubleLeftSpaces . toUpperRadixTokens 16 2),
    ("char", singleLeftSpaces . mapRights chrString),
    ("alpha", singleLeftSpaces . toAlphaStream),
    ("Alpha", singleLeftSpaces . toUpperAlphaStream),
    ("number", doubleLeftSpaces . mapRights (unwords . map show) . groupRights),
    ("base64", toBase64Codex)
    ]

unpluralize :: String -> Maybe String
unpluralize s = case last s of
    's' -> Just $ init s
    _   -> Nothing

look :: M.Map String v -> String -> String -> Either String v
look m s em = case catMaybes [M.lookup s m, (`M.lookup` m) =<< unpluralize s] of
    [] -> Left em
    (x:_) -> Right x

data ArgToken = IntToken Int | WordToken String deriving (Eq, Show)

readArgToken :: String -> ArgToken
readArgToken str = maybe (WordToken str) IntToken (readMaybe str :: Maybe Int)

parseModifier :: [ArgToken] -> Either String (CodexList -> CodexList)
parseModifier [] = Right id
parseModifier (WordToken "plus" : IntToken n : ts) = (. mapRights (+n)) <$> parseModifier ts
parseModifier (WordToken "minus" : IntToken n : ts) = (. mapRights (subtract n)) <$> parseModifier ts
parseModifier (WordToken "times" : IntToken n : ts) = (. mapRights (*n)) <$> parseModifier ts
parseModifier (WordToken "shift" : IntToken n : ts) = (. mapRights ((`mod1` 26) . (+n))) <$> parseModifier ts
parseModifier (WordToken "rot13" : ts) = (. mapRights ((`mod1` 26) . (+13))) <$> parseModifier ts
parseModifier _ = Left "Could not parse extra modifiers"

parseReader :: [ArgToken] -> Either String (String -> CodexList)
parseReader (WordToken s : rs) = do
    rd <- look plainReaderList s "No such reader without argument"
    mf <- parseModifier rs
    return $ mf . rd
parseReader (IntToken n : WordToken s : rs) = do
    rd <- look argReaderList s "No such reader with argument"
    mf <- parseModifier rs
    return $ mf . rd n
parseReader _ = Left "Could not parse reader"

parseWriter :: [ArgToken] -> Either String (CodexList -> CodexOutput)
parseWriter [WordToken s] = look plainWriterList s "No such writer without argument"
parseWriter [IntToken n, WordToken s] = ($ n) <$> look argWriterList s "No such writer without argument"
parseWriter _ = Left "Could not parse writer"

splitOn :: Eq a => a -> [a] -> ([a],[a])
splitOn x = second tail . span (/= x)

extractReader :: [String] -> Either String (String -> CodexList)
extractReader = parseReader . map readArgToken

extractWriter :: [String] -> Either String (CodexList -> CodexOutput)
extractWriter = parseWriter . map readArgToken

output :: CodexOutput -> String
output = concatMap (either id id)

codex :: [String] -> Either String (String -> String)
codex ["rot13"] = Right $ output . toAlphaStream . mapRights ((`mod1` 26) . (+13)) . fromAlphaStream
codex args = let (rargs, wargs) = splitOn "to" args in do
    rf <- extractReader rargs
    wf <- extractWriter wargs
    return $ output . wf . rf

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
    ]
    where codexw = either error id . codex . words

main :: IO ()
main = do
    args <- getArgs
    if args == ["test"] then void (runTestTT tests) else
        case codex args of
            Left em -> error em
            Right f -> interact $ unlines . map f . lines
