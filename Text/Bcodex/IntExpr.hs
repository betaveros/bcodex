module Text.Bcodex.IntExpr (parseIntExpr) where

import Text.Read (readMaybe)

-- Parse an expression such as "1", "-1", "i", "3+4i", into a list of values
-- produced by substiting 0, 1 etc. for i.  Right now, only very simple
-- expressions are supported: sums of constants and multiples of i.

parseIntAtom :: String -> Maybe [Int]
parseIntAtom "" = Just $ repeat 0
parseIntAtom "i" = Just [0..]
parseIntAtom s
    | Just n <- readMaybe s = Just $ repeat n
    | Just n <- readMaybe $ init s, 'i' <- last s = Just [0,n..]
    | otherwise = Nothing

-- modifier will be id or negate
parseModifiedIntExpr :: (Int -> Int) -> String -> Maybe [Int]
parseModifiedIntExpr modifier s = case break (`elem` "+-") s of
    (atom, "") -> map modifier <$> parseIntAtom atom
    (atom, sign:s') -> do
        atomList <- map modifier <$> parseIntAtom atom
        let modifier' = case sign of
                '+' -> id
                '-' -> negate
                _ -> error "impossible sign in parseModifiedIntExpr!"
        expr <- parseModifiedIntExpr modifier' s'
        return $ zipWith (+) atomList expr

parseIntExpr :: String -> Maybe [Int]
parseIntExpr = parseModifiedIntExpr id
