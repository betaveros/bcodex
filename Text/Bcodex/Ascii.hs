-- To be technically correct this module would be called
-- VisiblePrintableAscii. The main function we're interested in, mimicking
-- Alpha, is really just intended to make ROT47 possible.
module Text.Bcodex.Ascii (isPrintableAscii, mapUnderAscii, visiblePrintableChr, visiblePrintableChrString) where

import Text.Bcodex.Cx
import Data.Char (ord, chr)

isPrintableAscii :: Char -> Bool
isPrintableAscii ch = '!' <= ch && ch <= '~'

mapUnderAscii :: (Int -> Int) -> Char -> Char
mapUnderAscii f ch
    | '!' <= ch && ch <= '~' = chr $ ord '!' + f (ord ch - ord '!') `mod` 94
    | otherwise              = ch

visiblePrintableChr :: Int -> CxElem Char
visiblePrintableChr x
    | ord '!' <= x && x <= ord '~' = Right (chr x)
    | otherwise = Left (CxBadInt x)

visiblePrintableChrString :: Int -> CxElem String
visiblePrintableChrString = fmap (:[]) . visiblePrintableChr
