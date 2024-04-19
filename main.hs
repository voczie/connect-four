import Data.ByteString (find)
type Position = (Int, Int, Int) --Type where the first int is 'X' position, the second is 'Y' position and the third is the piece
                                --if the third int is '0' theres no piece, if '1' the piece belongs to player 1 and '2' is players 2 piece
createBoard :: Int -> Int -> [[Position]] --create the board with no pieces placed
createBoard numRows numCols = replicate numRows $ replicate numCols (0, 0, 0)

printBoard :: [[Position]] -> IO () --function to print the board
printBoard board = mapM_ putStrLn $ map (concatMap showPosition) board
  where
    showPosition (_, _, 0) = "_ " --no piece
    showPosition (_, _, 1) = "X " --players 1 piece
    showPosition (_, _, 2) = "O " --players 2 piece

-- Function to update the board with the player's move
updateBoard :: [[Position]] -> Int -> Int -> ([[Position]], Int)
updateBoard board x player = (take row board ++ [updatedRow] ++ drop (row+1) board, row)
  where
    row = findEmptyRow board x
    updatedRow = take x (board !! row) ++ [(x, row, player)] ++ drop (x+1) (board !! row)



findEmptyRow :: [[Position]] -> Int -> Int
findEmptyRow board col = length colPositions - 1 - length (takeWhile (\(_,_,piece) -> piece /= 0) (reverse colPositions))
  where colPositions = map (!! col) board

-- Verifica se há uma sequência vencedora a partir de uma posição específica
checkSequence :: [[Position]] -> Int -> Int -> Int -> Int -> Bool
checkSequence board x y dx dy =
  let player = (\(_, _, p) -> p) (board !! y !! x)
      isValidCoord i j = i >= 0 && i < length board && j >= 0 && j < length (head board)
      isValidPiece i j = isValidCoord i j && (\(_, _, p) -> p) (board !! i !! j) == player
      countSequence i j count
        | count >= 4 = True
        | isValidPiece i j = countSequence (i + dx) (j + dy) (count + 1)
        | otherwise = False
  in countSequence x y 1

-- Verifica se há uma vitória a partir de uma posição específica em todas as direções
checkWin :: [[Position]] -> Int -> Int -> Bool
checkWin board x y =
  any (\(dx, dy) -> checkSequence board x y dx dy) directions
  where
    directions = [(1, 0), (0, 1), (1, 1), (1, -1)]

-- Verifica se há uma vitória após o último movimento
checkVictory :: [[Position]] -> Int -> Int -> Bool
checkVictory board x y = checkWin board x y
  
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
          let x = read column :: Int

          let (updatedBoard, row) = updateBoard board x player
          putStrLn "Updated Board:"
          printBoard updatedBoard

          if checkVictory updatedBoard x row
            then putStrLn $ "Player " ++ show player ++ " wins!"
            else if all (\(_, _, piece) -> piece /= 0) (concat updatedBoard)
              then putStrLn "It's a draw!"
              else loop updatedBoard (if player == 1 then 2 else 1)

    loop board 1

    

    -- let updatedBoardDOIS = updateBoard updatedBoard x 1 --Testa colocar um segunda peça encima da primeira
    -- putStrLn "Updated Board2:"
    -- printBoard updatedBoardDOIS

--TODO: TURNO, CONDICAO DE VITORIA, USER ERROR ex: X = -2, COMENTARIOS deixar bunitinho
