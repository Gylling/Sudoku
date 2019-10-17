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

----- Important lists and constants
possibleActions = [Action (row,col) i | i <- [1..9],row <- [0..8],col <- [0..8]]

userMessages = ["\nWelcome to the Sudoku Game.\n",
                "\nMake your first move. Choose a row, a column, and a number.\n",
                "\nThe move was correct!\n",
                "\nThe chosen position is outside of the board.\n",
                "\nYou have entered a number higher than 9 or lower than 1. Please try again.\n",
                "\nThe position is already taken. You must choose an empty position.\n",
                "\nThe move is correct. You win!\n\nNext level!\nMake your first move. Choose a row, a column, and a number.\n",
                "\nToo many mistakes. You lost the game.\n",
                "\nThe move is not correct. Try again!\n",
                "\nYou must enter a positive digit.\n",
                "\nThe move is correct. \nYou completed all levels!\nMake your first move. Choose a row, a column, and a number.\n"]

exitCommands = ["quit", "Quit", "QUIT", "q", "Q", "exit", "EXIT", "Exit", "e", "E", "3"]


data Action = Action (Int, Int) Int            -- a move for a player is a pair of coordinates and an integer
         deriving (Ord,Eq)

type InternalState = ([[Int]],[[Int]],Int, Int)          -- ([board], [solved board], mistakes, difficulty)


{-
Contains game logic and validation of the provided information from the user.
-}
sudoku :: Action -> State -> (Result, Int)
sudoku (Action (row,col) move) (State  (board, winBoard, mistakes, diff))
    | (checkInput (row, col))             = (ContinueGame (State (board, winBoard, mistakes, diff)), 3)
    | (checkValue move)                   = (ContinueGame (State (board, winBoard, mistakes, diff)), 4)
    | (validatePos (row,col) board)       = (ContinueGame (State (board, winBoard, mistakes, diff)), 5)
    | (checkMove (row,col) move winBoard) = moveCorrect (Action (row,col) move) (State (board, winBoard, mistakes, diff))
    | (mistakes == 2)                     = (EndOfGame ((State (board, winBoard, mistakes, diff))), 7)
    | otherwise                           = (ContinueGame (State (board, winBoard, mistakes+1, diff)), 8)

{-
Validation methods
-}
checkInput :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> Bool
checkInput (row,col)= row >8 || row <0 || col >8 || col <0

checkValue :: (Ord a1, Num a1) => a1 -> Bool
checkValue val = val >9 || val <1

validatePos :: (Eq a, Num a) => (Int, Int) -> [[a]] -> Bool
validatePos (row,col) board = 0 /= board !! row !! col

checkMove :: Eq a => (Int, Int) -> a -> [[a]] -> Bool
checkMove (row,col) move board = board !! row !! col == move

-- Check if input is a digit
checkNum :: String -> Bool
checkNum = all isDigit

isInputDigit :: (String, String, String) -> Bool
isInputDigit (x,y,no) = ((checkNum x) && (checkNum y) && (checkNum no)) && (x /= "") && ((x /= "") && (y /= "") && (no /= ""))

-- check if move is the winning move
moveCorrect :: Num b => Action -> State -> (Result, b)
moveCorrect (Action (row,col) move) (State  (board, winBoard, mistakes, diff)) =
  if diff==2 && insertMove board move (row,col) == winBoard
    then (EndOfGame (State (board, winBoard, mistakes, mod (diff+1) 3)), 10)
  else if insertMove board move (row,col) == winBoard
    then (EndOfGame (State (board, winBoard, mistakes, mod (diff+1) 3)), 6)
  else
    (ContinueGame (State ((insertMove board move (row,col)), winBoard, mistakes, diff)), 2)

{-
Method to insert move on board.
-}
insertMove :: [[Int]] -> Int -> (Int, Int) -> [[Int]]
insertMove board move (row,col) =
  take row board ++
  [take col (board !! row) ++ [move] ++ drop (col + 1) (board !! row)] ++
  drop (row + 1) board


{-
Parse methods.
-}
strToInt :: String -> Int
strToInt s = read s :: Int

{-
Interface logic for main menu.
-}

game_start =
    do
        (displayMainMenu 0)
        level <- getLine --prompt for menu choice
        extGame level
        game_play ((ContinueGame (State (createBoard ((strToInt level)-1)))), 1)

{-
Method for printing the main menu.
-}
displayMainMenu :: Int -> IO ()
displayMainMenu code =
    do
        putStrLn(userMessages!!code)
        putStrLn("What level do you wish to play? 1. Easy, 2. Medium, 3. Difficult.")
        putStrLn("To exit write quit or exit at any point during the game.")

{-
Interface logic for game.
-}
game_play :: (Result, Int) -> IO Integer
game_play ((EndOfGame (State (board, winBoard, mistakes, diff))), code) = game_play ((ContinueGame (State (createBoard diff))), code) -- Game ended (either lost or won)
game_play ((ContinueGame state), code) = --Game ongoing
   do
      let State (board, winBoard, mistakes, difficulty) = state --getting individual components of state
      (displayCurrState code mistakes board difficulty)
      putStrLn("Choose row:")
      x <- getLine
      extGame x
      putStrLn("Choose column:")

      y <- getLine
      extGame y
      putStrLn("Choose number:")

      no <- getLine
      extGame no

      if (isInputDigit (x,y,no))
        then game_play (sudoku (Action ((strToInt x), (strToInt y)) (strToInt no)) (state))
      else
        game_play ((ContinueGame state), 9)

{-
Method for printing the current game state.
-}
displayCurrState :: (Num a1, Show a1, Show a2, Show a3) => Int -> a2 -> [[a3]] -> a1 -> IO ()
displayCurrState code mistakes board diff =
    do
        putStrLn("\n*****************************************************************")
        putStrLn(userMessages!!code)
        putStrLn("*****************************************************************")
        putStrLn("\nLevel: " ++ show (diff+1))
        putStrLn("\nMistakes: " ++ show mistakes)
        putStrLn("\nThis is your current sudoku board: ")
        putStrLn("___________________________________________________________________\n")
        putStrLn(printBoard board)
        putStrLn("___________________________________________________________________\n")

{-
Method for printing the sudoku board.
-}
printBoard :: Show a => [[a]] -> [Char]
printBoard board = concat [if y==9 then "\n" else " "++ show (board !! x !! y) | x <- [0..8], y <- [0..9]]

{-
Method for exit
-}
extGame :: [Char] -> IO ()
extGame inp =
  if elem inp exitCommands
    then do exitWith ExitSuccess
  else
    putStr("")

instance Show Action where
    show (Action (row,col) x) = show x ++ " inserted at " ++ show (row,col)
