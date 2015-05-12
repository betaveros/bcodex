module Text.Bcodex.Base64 (
    toBase64Char, fromBase64Char, isBase64Char,
    chunkStream, packChunks,
    to64Fragments, toBase64, toNBytes,
    fromBase64,
    toBase64Codex, fromBase64Codex) where
import Data.Char (ord, chr)
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR)
import Text.Bcodex.Cx
import Text.Bcodex.Utils

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
