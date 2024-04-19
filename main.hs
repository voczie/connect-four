import Data.ByteString (find)
import Data.Char
type Position = (Int, Int, Int) --Type where the first int is 'X' position, the second is 'Y' position and the third is the piece
                                --if the third int is '0' theres no piece, if '1' the piece belongs to player 1 and '2' is players 2 piece
createBoard :: Int -> Int -> [[Position]] --create the board with no pieces placed
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

findEmptyRow :: [[Position]] -> Int -> Int
findEmptyRow board col = length colPositions - 1 - length (takeWhile (\(_,_,piece) -> piece /= 0) (reverse colPositions))
  where colPositions = map (!! col) board

-- Verifica se há uma sequência vencedora a partir de uma posição específica
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

-- Verifica se há uma vitória a partir de uma posição específica em todas as direções
checkWin :: [[Position]] -> Int -> Int -> Bool
checkWin board row col =
  any (uncurry (checkSequence board row col)) directions
  where
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

main :: IO ()
main = do
    let numRows = 6
        numCols = 7
        board = createBoard numRows numCols
    putStrLn "Initial Board:"
    printBoard board

    let loop board player = do
          putStrLn $ "Player " ++ show player ++ "'s turn. Enter your move (column number):"
          column <- getLine
          let col = read column :: Int

          let (updatedBoard, row) = updateBoard board col player
          putStrLn "Updated Board:"
          printBoard updatedBoard

          putStrLn $ "row: " ++ [chr (row + ord '0')]
          putStrLn $ "col: " ++ [chr (col + ord '0')]

          if checkWin updatedBoard row col
            then putStrLn $ "Player " ++ show player ++ " wins!"
            else if all (all (\(_, _, piece) -> piece /= 0)) updatedBoard
              then putStrLn "It's a draw!"
              else loop updatedBoard (if player == 1 then 2 else 1)

    loop board 1



    -- let updatedBoardDOIS = updateBoard updatedBoard x 1 --Testa colocar um segunda peça encima da primeira
    -- putStrLn "Updated Board2:"
    -- printBoard updatedBoardDOIS

--TODO: TURNO, CONDICAO DE VITORIA, USER ERROR ex: X = -2, COMENTARIOS deixar bunitinho
