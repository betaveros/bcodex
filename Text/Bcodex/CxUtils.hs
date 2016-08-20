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
    freezeCharClass, freezeElemType,
    unfreezeCharClass, unfreezeElemType,
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

freezeCharClassInElem :: (String -> CxElem String) -> (Char -> Bool) -> String -> CxList String
freezeCharClassInElem reconstructor cc s =
    map (either reconstructor (Left . CxFrozen)) $ tokensOf cc s

unfreezeCharClassInElem :: (Char -> Bool) -> String -> CxList String
unfreezeCharClassInElem cc s =
    map (either (Left . CxFrozen) Right) $ tokensOf cc s

freezeCharClass :: (Char -> Bool) -> CxList String -> CxList String
freezeCharClass cc = concatMap go
    where go (Left (CxExtra s)) = freezeCharClassInElem (Left . CxExtra) cc s
          go (Left (CxDelim s)) = freezeCharClassInElem (Left . CxDelim) cc s
          go (Right s) = freezeCharClassInElem Right cc s
          go x = [x]

unfreezeCharClass :: (Char -> Bool) -> CxList String -> CxList String
unfreezeCharClass cc = concatMap go
    where go (Left (CxFrozen s)) = unfreezeCharClassInElem cc s
          go x = [x]

freezeElemType :: (CxElem String -> Bool) -> CxList String -> CxList String
freezeElemType et = map go
    where go e = if et e then freeze e else e
          freeze (Left (CxExtra s)) = Left (CxFrozen s)
          freeze (Left (CxDelim s)) = Left (CxFrozen s)
          freeze (Right s) = Left (CxFrozen s)
          freeze x = x

unfreezeElemType :: (CxElem String -> Bool) -> CxList String -> CxList String
unfreezeElemType et = map go
    where go e@(Left (CxFrozen s)) = if et e then Right s else e
          go x = x

mapExtraStringGroups :: (String -> String) -> CxList a -> CxList a
mapExtraStringGroups f = mapExtraStrings f . concatExtraStrings

shrinkExtraSpaces :: CxList a -> CxList a
shrinkExtraSpaces = mapExtraStringGroups shrinkSpaces

expandExtraSpaces :: CxList a -> CxList a
expandExtraSpaces = mapExtraStringGroups expandSpaces
