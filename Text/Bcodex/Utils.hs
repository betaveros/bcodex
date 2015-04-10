module Text.Bcodex.Utils (
    mapRights, mapLefts,
    bindRights, leftRight, mapLeftRight, groupRights, concatMapRights,
    intersperseBetweenRights,
    ungroupRights, isDelimiter,
    str1, chrString, tokensOf, splitInto,
    shrinkSpaces, expandSpaces
    ) where

import Control.Arrow (left, right)
import Data.Char (chr)
import Data.List (groupBy, unfoldr)
import Data.Function (on)

mapRights :: (Functor f) => (a -> b) -> f (Either c a) -> f (Either c b)
mapRights = fmap . right

mapLefts :: (Functor f) => (a -> b) -> f (Either a c) -> f (Either b c)
mapLefts = fmap . left

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

concatMapRights :: (b -> [Either a c]) -> [Either a b] -> [Either a c]
concatMapRights f = concatMap ff
    where ff (Right s) = f s
          ff (Left x)  = [Left x]

intersperseBetweenRights :: Either a b -> [Either a b] -> [Either a b]
intersperseBetweenRights _ [] = []
intersperseBetweenRights b (Left c : xs) = Left c : intersperseBetweenRights b xs
intersperseBetweenRights b (Right r : xs) =
    case intersperseBetweenRights b xs of
        (Right rs : gps) -> Right r : b : Right rs : gps
        gps -> Right r : gps

ungroupRights :: [Either a [b]] -> [Either a b]
ungroupRights = concatMap (either (\a -> [Left a]) (map Right))

isDelimiter :: String -> Bool
isDelimiter "," = True
isDelimiter s = all (== ' ') s

str1 :: Char -> String
str1 c = [c]

chrString :: Int -> String
chrString = str1 . chr

tokensOf :: (a -> Bool) -> [a] -> [Either [a] [a]]
tokensOf p ls
    = mapLeftRight (p . head) $ groupBy ((==) `on` p) ls

splitInto :: Int -> [a] -> [[a]]
splitInto n = takeWhile (not . null) . unfoldr (Just . splitAt n)

shrinkSpaces :: String -> String
shrinkSpaces (' ' : r@(' ':s)) | all (== ' ') s = r
shrinkSpaces s = s

expandSpaces :: String -> String
expandSpaces r@(' ' : s) | all (== ' ') s = ' ' : r
expandSpaces s = s
