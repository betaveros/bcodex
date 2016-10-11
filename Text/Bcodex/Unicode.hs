{-# LANGUAGE ViewPatterns #-}
module Text.Bcodex.Unicode (
    fullwidth, halfwidth, circled, uncircled) where

import Data.Char (chr, ord)

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
