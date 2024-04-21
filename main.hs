import Data.ByteString (find)
import Data.Char
type Position = (Int, Int, Int) --Type where the first int is 'X' position, the second is 'Y' position and the third is the piece
                                --if the third int is '0' theres no piece, if '1' the piece belongs to player 1 and '2' is players 2 piece

createBoard :: Int -> Int -> [[Position]] --create the board with no pieces placed using the numbers of rows and columns given
createBoard numRows numCols = replicate numRows $ replicate numCols (0, 0, 0)

getPosition :: [[Position]] -> Int -> Int -> Int
getPosition board row col = (\(_, _, p) -> p) (board !! row !! col)

printBoard :: [[Position]] -> IO () --function to print the board
printBoard = mapM_ (putStrLn . concatMap showPosition)
  where
    showPosition (_, _, 0) = "_ " --no piece
    showPosition (_, _, 1) = "X " --players 1 piece
    showPosition (_, _, 2) = "O " --players 2 piece

-- Function to update the board with the player's move
updateBoard :: [[Position]] -> Int -> Int -> ([[Position]], Int)
updateBoard board col player = (take row board ++ [updatedRow] ++ drop (row+1) board, row)
  where
    row = findEmptyRow board col
    updatedRow = take col (board !! row) ++ [(col, row, player)] ++ drop (col+1) (board !! row)

-- Checks the first available row of a column
findEmptyRow :: [[Position]] -> Int -> Int
findEmptyRow board col = length colPositions - 1 - length (takeWhile (\(_,_,piece) -> piece /= 0) (reverse colPositions))
  where colPositions = map (!! col) board

-- Checks if there is a winning sequence from the given position
checkSequence :: [[Position]] -> Int -> Int -> Int -> Int -> Bool
checkSequence board row col dx dy =
  let player = getPosition board row col
      isValidCoord i j = i >= 0 && i < length board && j >= 0 && j < length (head board)

      isValidPiece i j = isValidCoord i j && getPosition board i j == player

      countSequence i j count
        | count >= 4 = True
        | isValidPiece i j = countSequence (i + dx) (j + dy) (count + 1)
        | otherwise = False

  in countSequence row col 0

-- Checks if there is a winnig sequence, verifiying all directions from a specific position
checkWin :: [[Position]] -> Int -> Int -> Bool
checkWin board row col =
  any (uncurry (checkSequence board row col)) directions
  where
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

main :: IO ()
main = do
    let numRows = 6 --these are the original board dimensions
        numCols = 7
        board = createBoard numRows numCols
    putStrLn "Initial Board:" --print the initial, empty, board
    printBoard board

    let loop board player = do
          putStrLn $ "Player " ++ show player ++ "'s turn. Enter your move (column number):"
          column <- getLine --gets the column the player wants to place a piece
          let col = read column :: Int

          let (updatedBoard, row) = updateBoard board col player --updates the board with the new piece
          putStrLn "Updated Board:"
          printBoard updatedBoard

          putStrLn $ "row: " ++ [chr (row + ord '0')] --shows the row and column the piece was placed
          putStrLn $ "col: " ++ [chr (col + ord '0')]

          if checkWin updatedBoard row col --checks if the new piece wins the game
            then putStrLn $ "Player " ++ show player ++ " wins!"
            else if all (all (\(_, _, piece) -> piece /= 0)) updatedBoard --if the board is full thegame ends with a draw
              then putStrLn "It's a draw!"
              else loop updatedBoard (if player == 1 then 2 else 1)

    loop board 1