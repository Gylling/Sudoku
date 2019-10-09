-- CPSC 312 Project 1
-- Deina Kellezi & Erik Gylling

module Sudoku where

import SudokuBoard
import System.IO

{-
Type and data definitions.
-}
data State = State InternalState  -- internal_state
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

----- Sudoku

possibleActions = [Action (row,col) i | i <- [1..9],row <- [0..8],col <- [0..8]]

data Action = Action (Int, Int) Int            -- a move for a player is a pair of coordinates and an integer
         deriving (Ord,Eq)

type InternalState = ([[Int]],[[Int]],Int, Int)          -- ([board], [solved board], mistakes, difficulty)


sudoku :: Game
sudoku (Action (row,col) move) (State  (board, winBoard, mistakes, diff))
    | (board == winBoard) = EndOfGame 1 (State (board, winBoard, mistakes, diff))
    | (mistakes >= 3) = EndOfGame (-1) ((State (board, winBoard, mistakes, diff)))
    | (checkInput (row, col) move) = (ContinueGame (State (board, winBoard, mistakes, diff)))
    | (validatePos (row,col) board) = ContinueGame (State (board, winBoard, mistakes, diff))
    | (checkMove (row,col) move winBoard) = ContinueGame (State ((insertMove board move (row,col)), winBoard, mistakes, diff))
    | otherwise = ContinueGame (State (board, winBoard, mistakes+1, diff))

{-
Validation methods for the game logic.
-}    
checkInput :: (Ord a1, Ord a2, Ord a3, Num a1, Num a2, Num a3) => (a1, a2) -> a3 -> Bool
checkInput (row,col) move = row >8 || row <0 || col >8 || col <0 || move >9 || move <1

validatePos :: (Eq a, Num a) => (Int, Int) -> [[a]] -> Bool
validatePos (row,col) board = 0 == board !! row !! col

checkMove :: Eq a => (Int, Int) -> a -> [[a]] -> Bool
checkMove (row,col) move board = board !! row !! col == move

insertMove :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
insertMove board move (row,col) =
  take row board ++
  [take col (board !! row) ++ [move] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board


{-
Interface logic for main menu.
-}
game_start :: Double -> IO Integer
game_start winOrLose =
    do
        if winOrLose == 1
            then putStrLn("Congrats! You won the game. Let's play again.")
        else if winOrLose == (-1)
            then putStrLn("Sorry, too many mistakes. You lost. Try again.")
        else putStrLn("Welcome to Sudoku.")
        putStrLn("What level do you wish to play? 0. Easy, 1. Medium, 2. Difficult.")
        level <- getLine --prompt for menu choice
        if level == "3"
            then return (-1)
        else game_play (ContinueGame (State (createBoard level)))
           

{-
Interface logic for game.
Game :: The original information about the game.
State :: Current game state.
-}
game_play :: Result -> IO Integer
game_play (EndOfGame val state) = (game_start val) -- Game ended (either lost or won)
game_play (ContinueGame state) = --Game ongoing
   do
      let State (board, winBoard, mistakes, difficulty) = state --getting individual components of state
 --     putStrLn ("This is your current sudoku board: ")
   --   (printBoard board)
      putStrLn("Choose row:")
      x <- getLine
      putStrLn("Choose column:")
      y <- getLine
      putStrLn("Choose number:")
      no <- getLine
      game_play (sudoku (Action ((strToInt x), (strToInt y)) (strToInt no)) (state)) 

      
strToInt s = read s :: Int

--printBoard arr =
  --  unlines [unwords [show (arr ! (x, y)) | x <- [0..9]] | y <- [0..9]]


-- start state
--sudoku_start :: Int -> State
--sudoku_start diff = State (createBoard diff)

instance Show Action where
    show (Action (row,col) x) = show x ++ " inserted at " ++ show (row,col)

    --            then computer_play game (ContinueGame start_state) opponent (State (createBoard diff))