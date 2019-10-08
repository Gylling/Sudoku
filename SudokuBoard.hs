module SudokuBoard where

createBoard diff
    | diff == 0 =
                ([[0,5,8,2,4,0,9,1,0],
                  [0,0,0,0,9,0,6,8,7],
                  [0,0,0,0,6,0,2,0,0],
                  [8,0,5,0,0,0,4,0,0],
                  [0,7,0,0,5,0,1,6,2],
                  [1,2,0,0,0,4,0,3,0],
                  [0,9,6,0,8,1,3,0,5],
                  [0,8,1,0,0,0,0,2,0],
                  [7,4,3,5,0,6,0,0,0]],

                  [[6,5,8,2,4,7,9,1,3],
                  [4,3,2,1,9,5,6,8,7],
                  [9,1,7,8,6,3,2,5,4],
                  [8,6,5,3,1,2,4,7,9],
                  [3,7,4,9,5,8,1,6,2],
                  [1,2,9,6,7,4,5,3,8],
                  [2,9,6,7,8,1,3,4,5],
                  [5,8,1,4,3,9,7,2,6],
                  [7,4,3,5,2,6,8,9,1]],

                  0, diff)
    | diff == 1 =
                ([[0,0,1,9,0,6,0,0,5],
                  [3,0,0,8,0,0,0,2,0],
                  [0,5,6,4,0,0,0,0,0],
                  [7,0,0,0,0,9,5,0,0],
                  [0,0,0,0,0,0,1,6,2],
                  [0,3,0,0,0,0,0,7,0],
                  [4,0,7,0,0,8,9,0,3],
                  [6,8,0,1,0,0,0,0,0],
                  [9,0,0,0,4,0,6,0,7]],

                 [[8,7,1,9,2,6,3,4,5],
                  [3,4,9,8,5,1,7,2,6],
                  [2,5,6,4,7,3,8,9,1],
                  [7,6,4,2,1,9,5,3,8],
                  [5,9,8,7,3,4,1,6,2],
                  [1,3,2,6,8,5,4,7,9],
                  [4,2,7,5,6,8,9,1,3],
                  [6,8,3,1,9,7,2,5,4],
                  [9,1,5,3,4,2,6,8,7]],

                  0, diff)
    | diff == 2 =
                ([[0,3,1,0,0,2,0,0,8],
                  [6,0,0,0,8,0,0,0,0],
                  [0,0,0,0,0,0,0,3,1],
                  [7,5,0,0,0,0,0,6,9],
                  [0,9,0,0,1,0,0,2,0],
                  [0,0,8,0,3,6,0,0,0],
                  [9,0,0,0,0,5,0,0,0],
                  [0,7,0,2,0,0,0,0,0],
                  [1,0,0,6,0,0,4,0,0]],

                 [[5,3,1,7,6,2,9,4,8],
                  [6,4,9,3,8,1,2,7,5],
                  [8,2,7,4,5,9,6,3,1],
                  [7,5,3,8,2,4,1,6,9],
                  [4,9,6,5,1,7,8,2,3],
                  [2,1,8,9,3,6,7,5,4],
                  [9,6,2,1,4,5,3,8,7],
                  [3,7,4,2,9,8,5,1,6],
                  [1,8,5,6,7,3,4,9,2]],

                  0, diff)
    | otherwise =
                ([[0,5,8,2,4,0,9,1,0],
                  [0,0,0,0,9,0,6,8,7],
                  [0,0,0,0,6,0,2,0,0],
                  [8,0,5,0,0,0,4,0,0],
                  [0,7,0,0,5,0,1,6,2],
                  [1,2,0,0,0,4,0,3,0],
                  [0,9,6,0,8,1,3,0,5],
                  [0,8,1,0,0,0,0,2,0],
                  [7,4,3,5,0,6,0,0,0]],

                  [[6,5,8,2,4,7,9,1,3],
                  [4,3,2,1,9,5,6,8,7],
                  [9,1,7,8,6,3,2,5,4],
                  [8,6,5,3,1,2,4,7,9],
                  [3,7,4,9,5,8,1,6,2],
                  [1,2,9,6,7,4,5,3,8],
                  [2,9,6,7,8,1,3,4,5],
                  [5,8,1,4,3,9,7,2,6],
                  [7,4,3,5,2,6,8,9,1]],

                  0, 0)
