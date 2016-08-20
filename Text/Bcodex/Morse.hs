module Text.Bcodex.Morse (
    morseTable, toMorseMap, morseSpace, toMorse, toMorseCodex,
    fromMorseMap, fromMorse, fromMorseCodex) where
import qualified Data.Map as Map
import Data.Char (toLower)
import Data.Tuple (swap)
import Text.Bcodex.Cx
import Text.Bcodex.Utils
import Text.Bcodex.CxUtils

morseTable :: [(Char, String)]
morseTable =
    [ ('a', ".-"  ), ('b', "-..."), ('c', "-.-."), ('d', "-.." ), ('e', "."   )
    , ('f', "..-."), ('g', "--." ), ('h', "...."), ('i', ".."  ), ('j', ".---")
    , ('k', "-.-" ), ('l', ".-.."), ('m', "--"  ), ('n', "-."  ), ('o', "---" )
    , ('p', ".--."), ('q', "--.-"), ('r', ".-." ), ('s', "..." ), ('t', "-"   )
    , ('u', "..-" ), ('v', "...-"), ('w', ".--" ), ('x', "-..-"), ('y', "-.--")
    , ('z', "--..")
    , ('0', "-----"), ('1', ".----"), ('2', "..---"), ('3', "...--"), ('4', "....-")
    , ('5', "....."), ('6', "-...."), ('7', "--..."), ('8', "---.."), ('9', "----.")
    , (',', "--..--"), ('.', ".-.-.-"), ('?', "..--.."), (';', "-.-.-.")
    , (':', "---..."), ('\'', ".----."), ('-', "-....-"), ('/', "-..-.")
    , ('(', "-.--."), (')', "-.--.-"), ('_', "..--.-")
    ]

toMorseMap :: Map.Map Char String
toMorseMap = Map.fromList morseTable

morseSpace :: CxLeft
morseSpace = CxExtra " / "

toMorse :: Char -> CxElem String
toMorse ' ' = Left morseSpace
toMorse c = case Map.lookup (toLower c) toMorseMap of
    Just s -> Right s
    Nothing -> Left $ CxExtra [c]

toMorseCodex :: CxList Char -> CxList Char
toMorseCodex = ungroupWithDelimSpaces . mapLefts f . bindRights toMorse
    where f (CxDelim " ") = morseSpace
          f x = x

fromMorseMap :: Map.Map String Char
fromMorseMap = Map.fromList $ map swap morseTable

fromMorse :: String -> CxElem Char
fromMorse s = case Map.lookup s fromMorseMap of
    Just c -> Right c
    Nothing -> Left $ CxBadString s

fromMorseCodex :: CxList Char -> CxList Char
fromMorseCodex = bindRights fromMorse . crunchMorseDelimiterLefts . concatMapGroupedRights (mapLefts CxExtra . tokensOf (`elem` ".-"))
