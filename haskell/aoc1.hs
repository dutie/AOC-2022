import System.IO
import Data.List


main = do
    contents <- readFile "../data/data1.txt"
    let dataArray = lines contents
    let temp = createArrays dataArray []
    let f = map (map read) temp :: [[Int]]
    let s = map (sum) f
    let r = reverse $ sort s
    print $ head r 
    print $ sum $ take 3 r

createArrays [] acc = [acc]
createArrays (x:xs) acc
    | x == ""   = acc : createArrays xs []
    | otherwise = createArrays xs rev where rev = x : reverse acc


