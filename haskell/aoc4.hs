import System.IO
import Data.List

main = do
    contents <- lines <$> readFile "../data/data4.txt"
    let parsed = map (\x -> toLongList $ parseString x) contents
    print $ length . filter (== True) $ map fullyContains parsed
    print $ length . filter (== True) $ map overlaps parsed


split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim

parseString :: String -> [[Int]]
parseString list = map (map read) $ map (\x -> split x '-') $ split list ','    

toLongList :: [[Int]] -> [[Int]]
toLongList [] = []
toLongList (xs:xss) = [x..y] : toLongList xss where
    [x,y] = xs
-- parseString :: String -> (String, String) -> [[Int]]
-- parseString (s:ss) (left, right)
--     | ss == ""  = [[(read $ reverse right) .. (read $ reverse (s:left))]]
--     | s == '-'  = parseString ss (right, left)
--     | s == ','  = [(read $ reverse right), (read $ reverse left)] : parseString ss ("", "")
--     | otherwise = parseString ss (s:left, right)

fullyContains :: [[Int]] -> Bool
fullyContains l = foldl1 intersect l `elem` l

overlaps :: [[Int]] -> Bool
overlaps = not . null . foldl1 intersect
