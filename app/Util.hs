module Util where

--- utils (NICHT AENDERN!)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] = const True 
isPrefixOf pre = (==pre) . take (length pre)

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix pre txt
    | pre `isPrefixOf` txt = drop (length pre) txt
    | otherwise = txt

-- | Split a list into consecutive sublists, separated by the same element.
--
-- >>> splitOn ',' "foo,foobar,foobarbaz,"
-- ergibt ["foo", "foobar", "foobarbaz", ""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x ys = case break (==x) ys of
    (ys1, []) -> [ys1]
    (ys1, _:ys2) -> ys1 : splitOn x ys2
