module Text.Bcodex.Html (ampEscape, showCxLeftAsHtml, codexHtml) where

import Text.Bcodex

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

applyCxCoderToStringAsHtml :: CxCoder a -> a -> String
applyCxCoderToStringAsHtml c = case c of
        Left f  -> go (classed "ri" . show) f
        Right f -> go (classed "rs") f
    where go showRight f = concatMap (either showCxLeftAsHtml showRight) . f . (:[]) . Right

codexHtml :: [String] -> Either String (String -> String)
codexHtml args = applyCxCoderToStringAsHtml <$> parseStringCoder args
