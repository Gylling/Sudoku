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

mistakeTypes = ["\nWelcome to the Sudoku Game.\n", "\nMake your first move. Choose a row, a column, and a number.\n", "\nThe move was correct!\n", "\nTEST TEST TEST.\n", "\nThe chosen position is outside of the board, or you have entered a number higher than 9. Please try again.\n", "\nThe position is already taken. You must choose an empty position.\n", "\nThe move is not correct\n", "\nToo many mistakes. You lost the game.\n", "\nThe move is not correct. Try again!\n"]

data Action = Action (Int, Int) Int            -- a move for a player is a pair of coordinates and an integer
         deriving (Ord,Eq)

type InternalState = ([[Int]],[[Int]],Int, Int)          -- ([board], [solved board], mistakes, difficulty)


sudoku :: Action -> State -> (Result, Int)
sudoku (Action (row,col) move) (State  (board, winBoard, mistakes, diff))
    | (board == winBoard)                 = (EndOfGame 1 (State (board, winBoard, mistakes, diff)), 2)
    | (mistakes > 2)                      = (EndOfGame (-1) ((State (board, winBoard, mistakes, diff))), 3)
    | (checkInput (row, col) move)        = (ContinueGame (State (board, winBoard, mistakes, diff)), 4)
    | (validatePos (row,col) board)       = (ContinueGame (State (board, winBoard, mistakes, diff)), 5)
    | (checkMove (row,col) move winBoard) = (ContinueGame (State ((insertMove board move (row,col)), winBoard, mistakes, diff)), 6)
    | (mistakes == 2)                     = (EndOfGame (-1) ((State (board, winBoard, mistakes, diff))), 7)
    | otherwise                           = (ContinueGame (State (board, winBoard, mistakes+1, diff)), 8)

{-
Validation methods for the game logic.
-}
checkInput :: (Ord a1, Ord a2, Ord a3, Num a1, Num a2, Num a3) => (a1, a2) -> a3 -> Bool
checkInput (row,col) move = row >8 || row <0 || col >8 || col <0 || move >9 || move <1

validatePos :: (Eq a, Num a) => (Int, Int) -> [[a]] -> Bool
validatePos (row,col) board = 0 /= board !! row !! col

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
game_start :: Int -> IO Integer
game_start code =
    do
        putStrLn(mistakeTypes!!code)
        putStrLn("What level do you wish to play? 0. Easy, 1. Medium, 2. Difficult.")
        putStrLn("To exit, press 3.")
        level <- getLine --prompt for menu choice
        if level == "3"
            then return (-1)
        else game_play ((ContinueGame (State (createBoard level))), 1)


{-
Interface logic for game.
Game :: The original information about the game.
State :: Current game state.
-}
game_play :: (Result, Int) -> IO Integer
game_play ((EndOfGame val state), code) = (game_start code) -- Game ended (either lost or won)
game_play ((ContinueGame state), code) = --Game ongoing
   do
      let State (board, winBoard, mistakes, difficulty) = state --getting individual components of state
      putStrLn((mistakeTypes!!code))
      putStrLn ("Mistakes: " ++ show mistakes)
      putStrLn ("This is your current sudoku board: ")
      putStrLn (printBoard board)
      putStrLn("Choose row:")
      x <- getLine
      putStrLn("Choose column:")
      y <- getLine
      putStrLn("Choose number:")
      no <- getLine
      game_play (sudoku (Action ((strToInt x), (strToInt y)) (strToInt no)) (state))


strToInt s = read s :: Int

printBoard board = concat [if y==9 then "\n" else " "++ show (board !! x !! y) | x <- [0..8], y <- [0..9]]


-- start state
--sudoku_start :: Int -> State
--sudoku_start diff = State (createBoard diff)

instance Show Action where
    show (Action (row,col) x) = show x ++ " inserted at " ++ show (row,col)

    --            then computer_play game (ContinueGame start_state) opponent (State (createBoard diff))