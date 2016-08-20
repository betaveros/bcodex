module Text.Bcodex.CxUtils (
    CxLeft(..), CxElem, CxList, showCxLeft,
    mapExtraStrings,
    intersperseDelimSpaces, ungroupWithDelimSpaces,
    concatExtraStrings,
    extraOrDelim,
    delimOrShrink,
    crunchDelimiterLefts,
    crunchMorseDelimiterLefts,
    concatMapAllChars,
    mapAllChars,
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

ungroupWithDelimSpaces :: CxList [a] -> CxList a
ungroupWithDelimSpaces = ungroupRights . intersperseDelimSpaces

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

concatMapAllChars :: (Char -> String) -> CxList Char -> CxList Char
concatMapAllChars f = concatMap f'
    where f' (Left  (CxBadString s)) = wrap (Left . CxBadString) $ cmf s
          f' (Left  (CxExtra     s)) = wrap (Left . CxExtra)     $ cmf s
          f' (Left  (CxDelim     s)) = wrap (Left . CxDelim)     $ cmf s
          f' (Right c) = map Right $ f c
          f' x = [x]
          cmf = concatMap f
          wrap c r = [c r | not (null r)]

mapAllChars :: (Char -> Char) -> CxList Char -> CxList Char
mapAllChars f = map f'
    where f' (Left  (CxBadString s)) = Left (CxBadString (map f s))
          f' (Left  (CxExtra     s)) = Left (CxExtra     (map f s))
          f' (Left  (CxDelim     s)) = Left (CxDelim     (map f s))
          f' (Right s) = Right (f s)
          f' x = x

freezeCharClassInElem :: (String -> CxElem Char) -> (Char -> Bool) -> String -> CxList Char
freezeCharClassInElem reconstructor cc s =
    concatMap (either ((:[]) . reconstructor) (map (Left . CxFrozen))) $ tokensOf cc s

freezeCharClass :: (Char -> Bool) -> CxList Char -> CxList Char
freezeCharClass cc = concatMap go
    where go (Left (CxExtra s)) = freezeCharClassInElem (Left . CxExtra) cc s
          go (Left (CxDelim s)) = freezeCharClassInElem (Left . CxDelim) cc s
          go e@(Right c) = [if cc c then Left (CxFrozen c) else e]
          go x = [x]

unfreezeCharClass :: (Char -> Bool) -> CxList Char -> CxList Char
unfreezeCharClass cc = map go
    where go e@(Left (CxFrozen c)) = if cc c then Right c else e
          go x = x

freezeElemType :: (CxElem Char -> Bool) -> CxList Char -> CxList Char
freezeElemType et = concatMap go
    where go e = if et e then freeze e else [e]
          freeze (Left (CxExtra s)) = map (Left . CxFrozen) s
          freeze (Left (CxDelim s)) = map (Left . CxFrozen) s
          freeze (Right c) = [Left (CxFrozen c)]
          freeze x = [x]

unfreezeElemType :: (CxElem Char -> Bool) -> CxList Char -> CxList Char
unfreezeElemType et = map go
    where go e@(Left (CxFrozen c)) = if et e then Right c else e
          go x = x

mapExtraStringGroups :: (String -> String) -> CxList a -> CxList a
mapExtraStringGroups f = mapExtraStrings f . concatExtraStrings

shrinkExtraSpaces :: CxList a -> CxList a
shrinkExtraSpaces = mapExtraStringGroups shrinkSpaces

expandExtraSpaces :: CxList a -> CxList a
expandExtraSpaces = mapExtraStringGroups expandSpaces
