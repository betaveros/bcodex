-- To be technically correct this module would be called
-- VisiblePrintableAscii. The only function we're interested in, mimicking
-- Alpha, is really just intended to make ROT47 possible.
module Text.Bcodex.Ascii (mapUnderAscii) where
import Data.Char (ord, chr)

mapUnderAscii :: (Int -> Int) -> Char -> Char
mapUnderAscii f ch
    | '!' <= ch && ch <= '~' = chr $ ord '!' + f (ord ch - ord '!') `mod` 94
    | otherwise              = ch
