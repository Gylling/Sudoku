-- CPSC 312 Project 1
-- Deina Kellezi & Erik Gylling

module Sudoku where

import SudokuBoard
import System.IO
import System.Exit
import Data.Char(isDigit)

{-
Type and data definitions.
-}
data State = State InternalState  -- internal_state
         deriving (Ord, Eq, Show)

data Result = EndOfGame State           -- end of game: starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

----- Sudoku

possibleActions = [Action (row,col) i | i <- [1..9],row <- [0..8],col <- [0..8]]

userMessages = ["\nWelcome to the Sudoku Game.\n", "\nMake your first move. Choose a row, a column, and a number.\n", "\nThe move was correct!\n", "\nThe chosen position is outside of the board.\n", "\nYou have entered a number higher than 9. Please try again.\n", "\nThe position is already taken. You must choose an empty position.\n", "\nThe move is not correct\n", "\nToo many mistakes. You lost the game.\n", "\nThe move is not correct. Try again!\n", "\nYou must enter a digit.\n"]

exitCommands = ["quit", "Quit", "QUIT", "q", "Q", "exit", "EXIT", "Exit", "e", "E"]


data Action = Action (Int, Int) Int            -- a move for a player is a pair of coordinates and an integer
         deriving (Ord,Eq)

type InternalState = ([[Int]],[[Int]],Int, Int)          -- ([board], [solved board], mistakes, difficulty)


sudoku :: Action -> State -> (Result, Int)
sudoku (Action (row,col) move) (State  (board, winBoard, mistakes, diff))
    | (board == winBoard)                 = (EndOfGame (State (board, winBoard, mistakes, diff)), 2)
    | (mistakes > 2)                      = (EndOfGame ((State (board, winBoard, mistakes, diff))), 3)
    | (checkInput (row, col))             = (ContinueGame (State (board, winBoard, mistakes, diff)), 3)
    | (checkValue move)                   = (ContinueGame (State (board, winBoard, mistakes, diff)), 4)
    | (validatePos (row,col) board)       = (ContinueGame (State (board, winBoard, mistakes, diff)), 5)
    | (checkMove (row,col) move winBoard) = (ContinueGame (State ((insertMove board move (row,col)), winBoard, mistakes, diff)), 6)
    | (mistakes == 2)                     = (EndOfGame ((State (board, winBoard, mistakes, diff))), 7)
    | otherwise                           = (ContinueGame (State (board, winBoard, mistakes+1, diff)), 8)

{-
Validation methods for the game logic.
-}
checkInput :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> Bool
checkInput (row,col)= row >8 || row <0 || col >8 || col <0

checkValue :: (Ord a1, Num a1) => a1 -> Bool
checkValue val = val >9 || val <1

validatePos :: (Eq a, Num a) => (Int, Int) -> [[a]] -> Bool
validatePos (row,col) board = 0 /= board !! row !! col

checkMove :: Eq a => (Int, Int) -> a -> [[a]] -> Bool
checkMove (row,col) move board = board !! row !! col == move

insertMove :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
insertMove board move (row,col) =
  take row board ++
  [take col (board !! row) ++ [move] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board

checkNum :: String -> Bool
checkNum = all isDigit

{-
Interface logic for main menu.
-}
game_start :: Int -> IO Integer
game_start code =
    do
        putStrLn(userMessages!!code)
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
game_play ((EndOfGame state), code) = (game_start code) -- Game ended (either lost or won)
game_play ((ContinueGame state), code) = --Game ongoing
   do
      let State (board, winBoard, mistakes, difficulty) = state --getting individual components of state
      putStrLn((userMessages!!code))
      putStrLn ("Mistakes: " ++ show mistakes)
      putStrLn ("This is your current sudoku board: ")
      putStrLn (printBoard board)
      putStrLn("Choose row:")
      x <- getLine
      if elem x exitCommands
        then do exitWith ExitSuccess
      else
        putStrLn("Choose column:")
      y <- getLine
      if elem y exitCommands
        then do exitWith ExitSuccess
      else
        putStrLn("Choose number:")
      no <- getLine
      if elem no exitCommands
        then do exitWith ExitSuccess
      else if ((checkNum x) && (checkNum y) && (checkNum no))
        then game_play (sudoku (Action ((strToInt x), (strToInt y)) (strToInt no)) (state))
      else 
        game_play ((ContinueGame state), 9)


strToInt s = read s :: Int

printBoard board = concat [if y==9 then "\n" else " "++ show (board !! x !! y) | x <- [0..8], y <- [0..9]]

-- start state
--sudoku_start :: Int -> State
--sudoku_start diff = State (createBoard diff)

instance Show Action where
    show (Action (row,col) x) = show x ++ " inserted at " ++ show (row,col)

    --            then computer_play game (ContinueGame start_state) opponent (State (createBoard diff))
