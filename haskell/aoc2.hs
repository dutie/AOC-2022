import System.IO
import Data.List

main = do
    contents <- readFile "../data/data2.txt"
    let line = lines contents
    let parsed = map parser line
    let score = sum $ map scoreCalculator parsed
    print $ score

data Game = Rock | Paper | Scissors deriving(Show, Eq)

data Beats = Win | Loss | Tie

parser :: String -> (Game, Game)
parser (o:_:r:rst) = (po, pr) where
    po = parserOpponent [o]
    pr = parserResponse [r]

parserOpponent :: String -> Game
parserOpponent "A" = Rock
parserOpponent "B" = Paper
parserOpponent "C" = Scissors

parserResponse :: String -> Game
parserResponse "X" = Rock
parserResponse "Y" = Paper
parserResponse "Z" = Scissors

scoreCalculator :: (Game, Game) -> Int
scoreCalculator (opponent, response) = (gameScore response) + (outcomeScore $ outcome opponent response)

gameScore :: Game -> Int
gameScore Rock     = 1
gameScore Paper    = 2
gameScore Scissors = 3

outcomeScore :: Beats -> Int
outcomeScore Win = 6
outcomeScore Tie = 3
outcomeScore Loss = 0

outcome :: Game -> Game -> Beats
outcome Rock Paper     = Win
outcome Paper Scissors = Win
outcome Scissors Rock  = Win
outcome opponent response 
    | opponent == response = Tie
    | otherwise            = Loss