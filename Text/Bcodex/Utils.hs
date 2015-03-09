module Text.Bcodex.Utils (
    CxLeft(..), CxElem, CxList, showCxLeft,
    mapRights, mapLefts, mapExtraStrings,
    bindRights, leftRight, mapLeftRight, groupRights, concatMapRights,
    intersperseBetweenRights, intersperseDelimSpaces,
    concatExtraStrings, ungroupRights, isDelimiter,
    extraOrDelim, delimOrShrink, crunchDelimiterLefts, crunchMorseDelimiterLefts,
    mapAllStrings,
    str1, chrString, tokensOf, splitInto,
    shrinkSpaces, expandSpaces, mapExtraStringGroups,
    shrinkExtraSpaces, expandExtraSpaces
    ) where

import Control.Arrow (left, right)
import Data.Char (chr)
import Data.Maybe (mapMaybe)
import Data.List (groupBy, unfoldr)
import Data.Function (on)
import Text.Bcodex.Cx

mapRights :: (Functor f) => (a -> b) -> f (Either c a) -> f (Either c b)
mapRights = fmap . right

mapLefts :: (Functor f) => (a -> b) -> f (Either a c) -> f (Either b c)
mapLefts = fmap . left

mapExtraStrings :: (Functor f) => (String -> String) -> f (CxElem c) -> f (CxElem c)
mapExtraStrings f = fmap f'
    where f' (Right r) = Right r
          f' (Left (CxExtra s)) = Left (CxExtra (f s))
          f' (Left x) = Left x

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
intersperseDelimSpaces :: CxList a -> CxList a
intersperseDelimSpaces = intersperseBetweenRights $ Left (CxDelim " ")

concatExtraStrings :: CxList a -> CxList a
concatExtraStrings [] = []
concatExtraStrings (Left (CxExtra r) : xs) =
    case concatExtraStrings xs of
        (Left (CxExtra r') : bss) -> Left (CxExtra (r ++ r')) : bss
        bss -> Left (CxExtra r) : bss
concatExtraStrings (x : xs) = x : concatExtraStrings xs

ungroupRights :: [Either a [b]] -> [Either a b]
ungroupRights = concatMap (either (\a -> [Left a]) (map Right))

isDelimiter :: String -> Bool
isDelimiter "," = True
isDelimiter s = all (== ' ') s

extraOrDelim :: String -> CxLeft
extraOrDelim s = if isDelimiter s then CxDelim s else CxExtra s

delimOrShrink :: String -> CxLeft
delimOrShrink s = case s of
    "" -> CxDelim ""
    " " -> CxDelim " "
    "," -> CxDelim ","
    (' ':r) | all (== ' ') r -> CxExtra r
    x -> CxExtra x

crunchDelimiterLefts :: CxList b -> CxList b
crunchDelimiterLefts = mapMaybe f
    where f (Left (CxDelim s)) =
            case s of
                "" -> Nothing
                " " -> Nothing
                "," -> Nothing
                (' ':x) -> Just (Left (CxExtra x))
                _ -> error "no such delimiter"
          f x = Just x

crunchMorseDelimiterLefts :: CxList b -> CxList b
crunchMorseDelimiterLefts = mapMaybe f
    where f (Left (CxExtra ""   )) = Nothing
          f (Left (CxDelim _    )) = Nothing
          f (Left (CxExtra " "  )) = Nothing
          f (Left (CxExtra " / ")) = Just (Left (CxExtra " "))
          f x = Just x

mapAllStrings :: (String -> String) -> CxList String -> CxList String
mapAllStrings f = map f'
    where f' (Left  (CxBadString s)) = Left (CxBadString (f s))
          f' (Left  (CxExtra     s)) = Left (CxExtra     (f s))
          f' (Left  (CxDelim     s)) = Left (CxDelim     (f s))
          f' (Right s) = Right (f s)
          f' x = x

-- list/string/character/digit utilities {{{
str1 :: Char -> String
str1 c = [c]

chrString :: Int -> String
chrString = str1 . chr

tokensOf :: (a -> Bool) -> [a] -> [Either [a] [a]]
tokensOf p ls
    = mapLeftRight (p . head) $ groupBy ((==) `on` p) ls

splitInto :: Int -> [a] -> [[a]]
splitInto n = takeWhile (not . null) . unfoldr (Just . splitAt n)
-- }}}

shrinkSpaces :: String -> String
shrinkSpaces (' ' : r@(' ':s)) | all (== ' ') s = r
shrinkSpaces s = s

expandSpaces :: String -> String
expandSpaces r@(' ' : s) | all (== ' ') s = ' ' : r
expandSpaces s = s

mapExtraStringGroups :: (String -> String) -> CxList a -> CxList a
mapExtraStringGroups f = mapExtraStrings f . concatExtraStrings

shrinkExtraSpaces :: CxList a -> CxList a
shrinkExtraSpaces = mapExtraStringGroups shrinkSpaces

expandExtraSpaces :: CxList a -> CxList a
expandExtraSpaces = mapExtraStringGroups expandSpaces

