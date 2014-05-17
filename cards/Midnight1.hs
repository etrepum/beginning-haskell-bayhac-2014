{-

# Midnight is a game played with six 6-sided dice.

## Gameplay

* On the first roll, all six dice are rolled.
* At the end of each roll, the player may choose
  to keep one or more dice.
* The turn ends when the player has no more dice
  to roll.

## Scoring

* To qualify, 1 and a 4 must be present.
* If the player qualifies, the total of the
  remaining four dice is the score

-}

import System.Random (randomRIO)
import Data.List ((\\))

type Die = Int
data Game = Game { rolledDice :: [Die]
                 , heldDice   :: [Die]
                 }
  deriving (Show)

main :: IO ()
main = do
  putStrLn "Maximum score is:"
  print (score [1,4,6,6,6,6])
  finalScore <- play initialGame
  putStrLn "Your final score:"
  print finalScore

initialGame :: Game
initialGame =
  Game { rolledDice = [1..6]
       , heldDice   = []
       }

rollDice :: [Die] -> IO [Die]
rollDice dice = mapM rollDie dice
  where rollDie _die = randomRIO (1, 6)

play :: Game -> IO (Maybe Int)
play game@(Game prevRoll held)
  | null prevRoll = return (score held)
  | otherwise = do
    rolled <- rollDice prevRoll
    putStrLn ("held  : " ++ show held)  
    putStrLn ("rolled: " ++ show rolled)
    game' <- askToKeep (game { rolledDice = rolled })
    play game'

askToKeep :: Game -> IO Game
askToKeep game = do
  putStrLn "Enter dice to keep separated by spaces and press return:"
  line <- getLine
  let dice = map read (words line)
  case keepDice game dice of
    Left err -> do
      putStrLn err
      askToKeep game
    Right game' ->
      return game'

score :: [Die] -> Maybe Int
score dice
  | 1 `elem` dice && 4 `elem` dice = Just (sum dice - 5)
  | otherwise                      = Nothing

keepDice :: Game -> [Die] -> Either String Game
keepDice (Game rolled held) dice
  | null dice =
    Left "You must hold at least one die"
  | length rolled == length rolled' + length dice =
    Right (Game rolled' (dice ++ held))
  | otherwise =
    Left "Not all dice were removed"
  where
    rolled' = rolled \\ dice
