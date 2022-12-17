import System.IO
import Data.List

main = do
    contents <- lines <$> readFile "../data/data3.txt"
    let pt1 = sum $ map (duplicateToPriority . parse) contents
    let pt2 = sum $ map (findBadges) $ sublist3 contents
    print $ pt1
    print $ pt2

sublist3 :: [String] -> [[String]]
sublist3 (x:y:z:[]) = [[x,y,z]]
sublist3 (x:y:z:rem) = [x,y,z] : sublist3 rem

parse :: String -> (String, String)
parse li = splitAt ((length li + 1) `div` 2) li

duplicateToPriority :: (String, String) -> Int
duplicateToPriority ((l:ls), rs)
    | (l:ls) == " " = 0
    | length findings == 0 = duplicateToPriority (ls, rs)
    | otherwise = parseToPriorities $ head findings where
        findings = filter (\ r -> l == r) rs

listLower = ['a'..'z']
listPrioritiesLower = [1..26]
listUpper = ['A'..'Z']
listPrioritiesUpper = [27..52]

parseToPriorities :: Char -> Int
parseToPriorities c 
    | elemIndex c listLower == Nothing = listPrioritiesUpper !! upperIndex
    | otherwise = listPrioritiesLower !!  lowerIndex where
        (Just upperIndex) =  elemIndex c listUpper 
        (Just lowerIndex) =  elemIndex c listLower

findBadges :: [String] -> Int
findBadges ((f:fst):snd:lst:[])
    | (f:fst)         == " " = 0
    | length findings == 0   = findBadges (fst:snd:lst:[])
    | otherwise              = parseToPriorities $ head findings where
        findings = [f | s <- snd, l <- lst, allSame f s l]

allSame :: Char -> Char -> Char -> Bool
allSame f s l = f == s && s == l