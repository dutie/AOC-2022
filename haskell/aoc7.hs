import System.IO
import Data.Function
import Data.Char
import Data.List

main = do
    input <- snd . flip parse (Folder "/" []) . drop 2 . lines<$> readFile "../data/data7.txt"
    let result = getFolderSizes input
    let p1 = sum $ filter (<= 100000) result
    let actualSpace = 70000000 - (head result)
    let toBeRemovedSpace = 30000000 - actualSpace
    let p2 = head  $ sort $ filter (>= toBeRemovedSpace) result
    print $ p1
    print $ toBeRemovedSpace
    print $ p2

data Directory = Folder String [Directory] | File String Int deriving(Show)

mkFolder :: String -> [Directory] -> Directory
mkFolder name others = Folder name others
mkFile :: String -> Int -> Directory
mkFile name size = File name size

parse :: [String] -> Directory -> ([String], Directory)
parse [] dir = ([],dir)
parse (x:xs) (Folder name subs)
    | x == "$ cd .."       = (xs, Folder name subs)
    | isPrefixOf "$ ls" x  = parse xs (Folder name subs)
    | isPrefixOf "dir " x  = parse xs (Folder name subs)
    | isPrefixOf "$ cd " x = parse xsNew (Folder name (folder : subs))
    | otherwise            = parse xs (Folder name (file : subs)) where
        [size, filename] = words x
        file = File filename (read size)
        (xsNew, folder) = parse xs (Folder (drop 5 x) []) 

getFileSize :: Directory -> Int
getFileSize (File _ size) = size
getFileSize (Folder _ fs) = sum $ map (getFileSize) fs

getFolderSizes :: Directory -> [Int]
getFolderSizes (File _  _) = []
getFolderSizes (Folder name subs) = getFileSize (Folder name subs) : foldl (\a d -> a ++ getFolderSizes d) [] subs