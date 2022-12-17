import System.IO
import Data.List

main = do
    contents <- readFile "../data/data2.txt"
    let line = lines contents
    let parsed = map parser line
    let score = sum $ map scoreCalculator parsed
    print $ score

data Game = Rock | Paper | Scissors deriving(Show, Eq)

data Beats = Win | Loss | Tie deriving(Show, Eq)

parser :: String -> (Game, Beats)
parser (o:_:r:rst) = (po, pr) where
    po = parserOpponent [o]
    pr = parserResponse [r]

parserOpponent :: String -> Game
parserOpponent "A" = Rock
parserOpponent "B" = Paper
parserOpponent "C" = Scissors

parserResponse :: String -> Beats
parserResponse "X" = Loss
parserResponse "Y" = Tie
parserResponse "Z" = Win

scoreCalculator :: (Game, Beats) -> Int
scoreCalculator (opponent, response) = (outcomeScore response) + (gameScore $ outcome opponent response)

gameScore :: Game -> Int
gameScore Rock     = 1
gameScore Paper    = 2
gameScore Scissors = 3

outcomeScore :: Beats -> Int
outcomeScore Win = 6
outcomeScore Tie = 3
outcomeScore Loss = 0

outcome :: Game -> Beats -> Game
outcome Rock Win     = Paper
outcome Paper Win    = Scissors
outcome Scissors Win = Rock
outcome opponent response 
    | response == Tie = opponent
    | otherwise       = outcome (outcome opponent Win) Win