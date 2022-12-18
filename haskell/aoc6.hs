import System.IO
import Data.Function
import Data.Char
import Data.List

main = do
    input <- zip [1 .. ] <$> readFile "../data/data6.txt"
    print $ fst . last $ firstDifferent 4 input
    print $ fst . last $ firstDifferent 14 input

firstDifferent :: Int -> [(Int, Char)] -> [(Int, Char)]
firstDifferent n =  head . dropWhile ((/= n) . length) .
                    map (nubBy (on (==) snd) . take n) . iterate tail

