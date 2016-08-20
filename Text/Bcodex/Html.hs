module Text.Bcodex.Html (ampEscape, showCxLeftAsHtml, codexHtml) where

import Text.Bcodex
import Data.List (intercalate)
import Text.Bcodex.Utils (groupRights)

ampEscape :: String -> String
ampEscape = concatMap esc
    where esc '<' = "&lt;"
          esc '>' = "&gt;"
          esc '&' = "&amp;"
          esc  c  = [c]

classed :: String -> String -> String
classed className text = concat [
    "<span class='", className, "'>", ampEscape text, "</span>"]

showCxLeftAsHtml :: CxLeft -> String
showCxLeftAsHtml (CxBadString s) = classed "bs" $ "{" ++ s ++ "}"
showCxLeftAsHtml (CxExtra s) = classed "ex" s
showCxLeftAsHtml (CxDelim s) = classed "de" s
showCxLeftAsHtml (CxBadInt n) = classed "bi" $ "[" ++ show n ++ "]"
showCxLeftAsHtml (CxFrozen c) = classed "fr" [c]

showIntsAsHtml :: [Int] -> String
showIntsAsHtml = classed "ri" . unwords . map show

showCharsAsHtml :: String -> String
showCharsAsHtml = classed "rs"

codexHtml :: [String] -> Either String (String -> String)
codexHtml args = applyCxCoderToString showCxLeftAsHtml showIntsAsHtml showCharsAsHtml <$> parseCharCoder args
