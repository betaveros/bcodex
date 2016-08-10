module Text.Bcodex.Cx (CxLeft(..), CxElem, CxList, showCxLeft,
    isCxLeftBad, isBad, isCxLeftNeutral, isNeutral,
    isCxLeftFrozen, isFrozen) where

data CxLeft = CxBadString String | CxExtra String | CxDelim String | CxBadInt Int | CxFrozen String deriving (Eq, Ord, Show)

showCxLeft :: CxLeft -> String
showCxLeft (CxBadString s) = "{" ++ s ++ "}"
showCxLeft (CxExtra s) = s
showCxLeft (CxDelim s) = s
showCxLeft (CxBadInt n) = "[" ++ show n ++ "]"
showCxLeft (CxFrozen s) = s

type CxElem a = Either CxLeft a
type CxList a = [CxElem a]

isCxLeftBad :: CxLeft -> Bool
isCxLeftBad (CxBadString _) = True
isCxLeftBad (CxBadInt _) = True
isCxLeftBad _ = False

isBad :: CxElem a -> Bool
isBad = either isCxLeftBad (const False)

isCxLeftNeutral :: CxLeft -> Bool
isCxLeftNeutral (CxDelim _) = True
isCxLeftNeutral (CxExtra _) = True
isCxLeftNeutral _ = False

isNeutral :: CxElem a -> Bool
isNeutral = either isCxLeftNeutral (const False)

isCxLeftFrozen :: CxLeft -> Bool
isCxLeftFrozen (CxFrozen _) = True
isCxLeftFrozen _ = False

isFrozen :: CxElem a -> Bool
isFrozen = either isCxLeftFrozen (const False)
