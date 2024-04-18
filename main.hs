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
updateBoard :: [[Position]] -> Int -> Int -> [[Position]]
updateBoard board x player = take row board ++ [updatedRow] ++ drop (row+1) board
  where
    row = findEmptyRow board x
    updatedRow = take x (board !! row) ++ [(x, row, player)] ++ drop (x+1) (board !! row)


findEmptyRow :: [[Position]] -> Int -> Int
findEmptyRow board col = length colPositions - 1 - length (takeWhile (\(_,_,piece) -> piece /= 0) (reverse colPositions))
  where colPositions = map (!! col) board
  
main :: IO ()
main = do
    let numRows = 6--criar e mostrar o board inicial(vazio)
        numCols = 7
        board = createBoard numRows numCols
    putStrLn "Initial Board:"
    printBoard board

    putStrLn "Enter your move (column number):"-- pedir posicao dos testes e associar com X
    column <- getLine
    let x = read column :: Int

    let updatedBoard = updateBoard board x 1--testa colocar a primeira peça
    putStrLn "Updated Board:"
    printBoard updatedBoard

    let updatedBoardDOIS = updateBoard updatedBoard x 1--testa colocar um segunda peça encima da primeira
    putStrLn "Updated Board2:"
    printBoard updatedBoardDOIS

--TODO: TURNO, CONDICAO DE VITORIA, USER ERROR ex: X = -2, COMENTARIOS deixar bunitinho
