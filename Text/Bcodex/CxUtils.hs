module Text.Bcodex.CxUtils (
    CxLeft(..), CxElem, CxList, showCxLeft,
    mapExtraStrings,
    intersperseDelimSpaces,
    concatExtraStrings,
    extraOrDelim,
    delimOrShrink,
    crunchDelimiterLefts,
    crunchMorseDelimiterLefts,
    mapAllStrings,
    mapExtraStringGroups, shrinkExtraSpaces, expandExtraSpaces) where

import Data.Maybe (mapMaybe)
import Text.Bcodex.Cx
import Text.Bcodex.Utils

mapExtraStrings :: (Functor f) => (String -> String) -> f (CxElem c) -> f (CxElem c)
mapExtraStrings f = fmap f'
    where f' (Right r) = Right r
          f' (Left (CxExtra s)) = Left (CxExtra (f s))
          f' (Left x) = Left x

intersperseDelimSpaces :: CxList a -> CxList a
intersperseDelimSpaces = intersperseBetweenRights $ Left (CxDelim " ")

concatExtraStrings :: CxList a -> CxList a
concatExtraStrings [] = []
concatExtraStrings (Left (CxExtra r) : xs) =
    case concatExtraStrings xs of
        (Left (CxExtra r') : bss) -> Left (CxExtra (r ++ r')) : bss
        bss -> Left (CxExtra r) : bss
concatExtraStrings (x : xs) = x : concatExtraStrings xs

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
          f (Left (CxExtra " / ")) = Just (Left (CxDelim " "))
          f x = Just x

mapAllStrings :: (String -> String) -> CxList String -> CxList String
mapAllStrings f = map f'
    where f' (Left  (CxBadString s)) = Left (CxBadString (f s))
          f' (Left  (CxExtra     s)) = Left (CxExtra     (f s))
          f' (Left  (CxDelim     s)) = Left (CxDelim     (f s))
          f' (Right s) = Right (f s)
          f' x = x

mapExtraStringGroups :: (String -> String) -> CxList a -> CxList a
mapExtraStringGroups f = mapExtraStrings f . concatExtraStrings

shrinkExtraSpaces :: CxList a -> CxList a
shrinkExtraSpaces = mapExtraStringGroups shrinkSpaces

expandExtraSpaces :: CxList a -> CxList a
expandExtraSpaces = mapExtraStringGroups expandSpaces
