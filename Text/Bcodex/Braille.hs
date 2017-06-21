module Text.Bcodex.Braille (
    toBraillePatternCodex, fromBraillePatternCodex,
    toBraille, fromBraille
    ) where
import Data.Char (ord, chr, toLower)
import Data.Tuple (swap)
import Text.Bcodex.Cx
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils

import qualified Data.Map as Map

toBraillePattern :: Int -> CxElem Char
toBraillePattern n
    | 0 <= n && n < 256 = Right $ chr (0x2800 + n)
    | otherwise         = Left . CxBadInt $ n

fromBraillePattern :: Char -> CxElem Int
fromBraillePattern ch
    | 0 <= n && n < 256 = Right n
    | otherwise         = Left $ CxExtra [ch]
    where n = ord ch - 0x2800

brailleTable :: [(Char, Int)]
brailleTable =
    [ ('a',  1), ('b',  3), ('c',  9), ('d', 25), ('e', 17), ('f', 11)
    , ('g', 27), ('h', 19), ('i', 10), ('j', 26), ('k',  5), ('l',  7)
    , ('m', 13), ('n', 29), ('o', 21), ('p', 15), ('q', 31), ('r', 23)
    , ('s', 14), ('t', 30), ('u', 37), ('v', 39), ('w', 58), ('x', 45)
    , ('y', 61), ('z', 53)
    ]

toBrailleMap :: Map.Map Char Int
toBrailleMap = Map.fromList brailleTable
fromBrailleMap :: Map.Map Int Char
fromBrailleMap = Map.fromList $ map swap brailleTable

toBraillePatternCodex :: CxList Int -> CxList Char
toBraillePatternCodex = bindRights toBraillePattern . crunchDelimiterLefts

fromBraillePatternCodex :: CxList Char -> CxList Int
fromBraillePatternCodex = concatExtraStrings . concatMapGroupedRights (map fromBraillePattern)

toBraille :: Char -> CxElem Char
toBraille c = case Map.lookup (toLower c) toBrailleMap of
    Just n -> toBraillePattern n
    Nothing -> Left $ CxExtra [c]

fromBraille :: Char -> CxElem Char
fromBraille c = case fromBraillePattern c of
    Right n -> case Map.lookup n fromBrailleMap of
        Just c' -> Right c'
        Nothing -> Left $ CxBadInt n
    Left lf -> Left lf
