{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex.Unicode (
    fullwidth, halfwidth, circled, uncircled,
    officialUnicodeSmallCaps, hackySmallCaps, unSmallCaps) where

import Control.Arrow (second)
import Data.Char (chr, ord)
import Data.Tuple (swap)
import qualified Data.Map as Map

fullwidth :: Char -> Char
fullwidth c
    | c == ' ' = chr 0x3000
    | '!' <= c && c <= '~' = chr (ord c + 0xFEE0)
    | otherwise = c

halfwidth :: Char -> Char
halfwidth c@(ord -> o)
    | o == 0x3000 = ' '
    | 0xFF01 <= o && o <= 0xFF5E = chr (o - 0xFEE0)
    | otherwise = c

circled :: Char -> Char
circled c
    | c == '0' = chr 0x24EA
    | '1' <= c && c <= '9' = chr (ord c + 0x242F)
    | 'A' <= c && c <= 'Z' = chr (ord c + 0x2475)
    | 'a' <= c && c <= 'z' = chr (ord c + 0x246F)
    | otherwise = c

uncircled :: Char -> Char
uncircled c@(ord -> o)
    | o == 0x24EA = '0'
    | 0x2460 <= o && o <= 0x2468 = chr (o - 0x242F)
    | 0x24B6 <= o && o <= 0x24CF = chr (o - 0x2475)
    | 0x24D0 <= o && o <= 0x24E9 = chr (o - 0x246F)
    | otherwise = c

officialUnicodeSmallCapsTable :: [(Char, Char)]
officialUnicodeSmallCapsTable = map (second chr)
    [ ('a', 0x1d00), ('b', 0x299), ('c', 0x1d04), ('d', 0x1d05), ('e', 0x1d07)
    , ('f', 0xa730), ('g', 0x262), ('h', 0x29c), ('i', 0x26a), ('j', 0x1d0a)
    , ('k', 0x1d0b), ('l', 0x29f), ('m', 0x1d0d), ('n', 0x274), ('o', 0x1d0f)
    , ('p', 0x1d18), ('q', 0x71), ('r', 0x280), ('s', 0xa731), ('t', 0x1d1b)
    , ('u', 0x1d1c), ('v', 0x1d20), ('w', 0x1d21), ('x', 0x78), ('y', 0x28f)
    , ('z', 0x1d22) ]

officialUnicodeSmallCapsMap :: Map.Map Char Char
officialUnicodeSmallCapsMap = Map.fromList officialUnicodeSmallCapsTable

smallCapsHacks :: [(Char, Char)]
smallCapsHacks = map (second chr)
    [ ('f', 0x493) -- CYRILLIC SMALL LETTER GHE WITH STROKE
    , ('q', 0x1EB) -- LATIN SMALL LETTER O WITH OGONEK
    , ('s', 0x73)  -- LATIN SMALL LETTER S (itself)
    ]

hackySmallCapsMap :: Map.Map Char Char
hackySmallCapsMap = Map.fromList (officialUnicodeSmallCapsTable ++
    smallCapsHacks)

unSmallCapsMap :: Map.Map Char Char
unSmallCapsMap = Map.fromList $ map swap (officialUnicodeSmallCapsTable ++
    smallCapsHacks)

officialUnicodeSmallCaps :: Char -> Char
officialUnicodeSmallCaps c =
    Map.findWithDefault c c officialUnicodeSmallCapsMap

hackySmallCaps :: Char -> Char
hackySmallCaps c =
    Map.findWithDefault c c hackySmallCapsMap

unSmallCaps :: Char -> Char
unSmallCaps c =
    Map.findWithDefault c c unSmallCapsMap
