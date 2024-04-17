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
updateBoard :: [[Position]] -> Int -> Int -> Int -> [[Position]]
updateBoard board x y player = take y board ++ [updatedRow] ++ drop (y+1) board
  where
    updatedRow = take x row ++ [(x, y, player)] ++ drop (x+1) row
    row = board !! y

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

    let updatedBoard = updateBoard board x (findEmptyRow board x) 1--testa colocar a primeira peça
    putStrLn "Updated Board:"--TODO? esse findEmptyRow acho q da pra deixar na prorpia funcao updateBoard, mais prático
    printBoard updatedBoard

    let updatedBoard2 = updateBoard updatedBoard x (findEmptyRow updatedBoard x) 1--testa colocar um segunda peça encima da primeira
    putStrLn "Updated Board2:"
    printBoard updatedBoard2

--TODO: TURNO, LINHA 37, CONDICAO DE VITORIA, COMENTARIOS deixar bunitinho
