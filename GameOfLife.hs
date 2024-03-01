module Life where

--------------------------------------------
-- Types and Data Definitions

type Position = (Int, Int) -- (row,col)

data Cell = Cell {
                position :: Position, -- position of cell
                state :: Bool         -- state: True=ALive, False=Dead
            }
     deriving Show

-- display of cell
cellDisplay :: Cell -> String
cellDisplay cell = if state cell then "0" else "*"

-- Represents the game board.
type Board = [[Cell]]

-- Represents a sequence of game board states.
type GameStates = [Board]

-------------------------------------------------------------
-- creates a 20*50 board where all the cells are dead

-- create a single row of the board
createRow :: Int -> Int -> [Cell]
createRow rowNum numCols = [Cell (rowNum, colNum) False | colNum <- [1..numCols]]

-- create the board
createBoard :: Int -> Int -> Board
createBoard numRows numCols = [createRow rowNum numCols | rowNum <- [1..numRows]]

-- display the board
displayBoard :: Board -> String
displayBoard board = unlines [concatMap cellDisplay row | row <- board]

-- display a welcome message
displayWelcome :: String
displayWelcome = ">>>> Welcome to Conway's Game of Life! <<<<\n"
               ++ ">>>> * means dead and 0 means alive \n"


-- makes a 20*50 board out of a string of 100 character
stringToBoard  :: [Char] -> Board
stringToBoard str = chunksOf 50 $ zipWith createCell [(x, y) | y <- [1..20], x <- [1..50]] str
    where
        createCell :: Position -> Char -> Cell
        createCell pos char = Cell { position = pos, state = (char == '0') }
        
        chunksOf _ [] = []
        chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Converts a string to a board with automatic dimensions.
stringToBoardAuto :: [Char] -> Board
stringToBoardAuto str = zipWith createRow [1..numRows] rows
    where
        rows :: [String]
        rows = lines str
        numRows :: Int
        numRows = length rows
        numCols :: Int
        numCols = if numRows > 0 then length (head rows) else 0

        createCell :: Position -> Char -> Cell
        createCell pos char = Cell { position = pos, state = (char == '0') }
        createRow :: Int -> String -> [Cell]
        createRow rowNum rowStr = zipWith (\colNum char -> createCell (rowNum, colNum) char) [1..] rowStr
--------------------------------------------------------------

-- game of life logic

-- Counts the number of live cells on the board.
countLives :: Board -> Int
countLives board = length $ filter (state) $ concat board

-- List of positions representing neighboring cells.
neighborMatrix :: [Position]
neighborMatrix = [
    (-1,-1), (-1,0), (-1,1),
    (0, -1),         ( 0,1),
    (1, -1), (1, 0), ( 1,1)]

-- Counts the number of live neighbors of a cell.
countLiveNeighbors :: Board -> Position -> Int
countLiveNeighbors board pos = length $ filter (\ cell -> state cell) (findNeighbors board pos)

-- Finds neighboring cells of a given cell.
findNeighbors :: Board -> Position -> [Cell]
findNeighbors board (row0, col0) = filter isNeighbor (concat board)
    where
        isNeighbor :: Cell -> Bool
        isNeighbor cell = (row - row0, col - col0) `elem` neighborMatrix
            where 
                (row, col) = position cell

-- Computes the next generation of the board based on Conway's rules.
nextGeneration :: Board -> Board
nextGeneration board = map (map updateCellState) board
    where
        updateCellState cell = Cell { position = position cell, state = determineNewState (state cell) (countLiveNeighbors board (position cell)) }
        determineNewState :: Bool -> Int -> Bool
        -- Rule 2: Any live cell with two or three live neighbors lives on to the next generation.
        determineNewState True 2  = True
        determineNewState True 3  = True
        -- Rule 1: Any live cell with fewer than two live neighbors dies, as if by underpopulation.
        -- Rule 3: Any live cell with more than three live neighbors dies, as if by overpopulation.
        determineNewState True _                  = False
        -- Rule 4: Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
        determineNewState False 3                 = True
        -- otherwise
        determineNewState False _                 = False

--------------------------------------------------------------
-- Program Execution


-- Function to read the board from a file
readBoardFromFile :: FilePath -> IO Board
readBoardFromFile filePath = do
    contents <- readFile filePath
    return $ stringToBoardAuto contents

-- Main game loop
gameLoop :: GameStates -> IO ()
gameLoop states = do
    let currentBoard = head states
    putStrLn $ displayBoard currentBoard
    putStrLn $ "Current Generation: " ++ (show (length states))
    putStrLn $ "Current Lives: " ++ (show $ countLives currentBoard)
    putStrLn "Press 'e' to evolve, 'w' to go back, 'q' to quit:"
    command <- getLine
    case command of
        "e" -> gameLoop $ nextGeneration currentBoard : states
        "w" -> gameLoop $ if length states > 1 then tail states else states
        "q" -> return ()
        _   -> gameLoop states -- If any other input, just repeat the loop without change


-- Modify the 'go' function to start the game based on file input
go :: IO ()
go = do
    putStrLn displayWelcome
    putStrLn "Enter the path to the initial state file:"
    filePath <- getLine
    initialBoard <- readBoardFromFile filePath
    gameLoop [initialBoard]


-- runs the program
-- >ghci
-- > load
-- > go 
-----------------------------------------------------------











