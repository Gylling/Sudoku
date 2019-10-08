-- CPSC 312 Project 1
-- Deina Kellezi & Erik Gylling

module Sudoku where

import SudokuBoard

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
sudoku (Action (row,col) move) (State  (board, winBoard,mistakes,diff))
    | board == winBoard = EndOfGame 1 (sudoku_start (mod diff 3))
    | mistakes >= 3 = EndOfGame (-1) (sudoku_start diff)
    | checkInput (row,col) move = ContinueGame(State (board, winBoard, mistakes, diff))
    | validatePos (row,col) board = ContinueGame(State (board, winBoard, mistakes, diff))
    | checkMove (row,col) move winBoard = ContinueGame (State ((insertMove board move (row,col)),winBoard, mistakes, diff))
    | otherwise = ContinueGame (State (board, winBoard, mistakes+1, diff))

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

-- start state
sudoku_start :: Int -> State
sudoku_start diff = State (createBoard diff)


instance Show Action where
    show (Action (row,col) x) = show x ++ " inserted at " ++ show (row,col)
