module Text.Bcodex.Cx (CxLeft(..), CxElem, CxList, showCxLeft) where

data CxLeft = CxBadString String | CxExtra String | CxDelim String | CxBadInt Int deriving (Eq, Ord, Show)

showCxLeft :: CxLeft -> String
showCxLeft (CxBadString s) = "{" ++ s ++ "}"
showCxLeft (CxExtra s) = s
showCxLeft (CxDelim s) = s
showCxLeft (CxBadInt n) = "[" ++ show n ++ "]"

type CxElem a = Either CxLeft a
type CxList a = [CxElem a]
