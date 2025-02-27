module Game.UI where

import Game.Logic
import Game.Estrutura
import Data.List (transpose)

-- Largura fixa para cada célula de dica
cellWidth :: Int
cellWidth = 2

-- Formata uma string com preenchimento à esquerda para que tenha largura fixa
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Formata uma lista de dicas em uma string com largura fixa
-- A largura total da string será: (maxSize * cellWidth)
formatHints :: Int -> [Int] -> String
formatHints maxSize hints =
    let hintStrs = map (padLeft cellWidth . show) hints
        padding  = replicate (cellWidth * (maxSize - length hints)) ' '
    in padding ++ concat hintStrs

-- Alinha as dicas das colunas em linhas (cabeçalho) para impressão
alignColHints :: [[Int]] -> [String]
alignColHints cols =
    let maxHintSize = maximum (map length cols)
        paddedCols = map (formatHints maxHintSize) cols
        -- Divide cada string em "células" de tamanho fixo
        splitCol :: String -> [String]
        splitCol [] = []
        splitCol s  = let (chunk, rest) = splitAt cellWidth s 
                      in chunk : splitCol rest
        splitCols = map splitCol paddedCols
        -- Transpõe para que cada linha do cabeçalho corresponda a uma "linha" de dicas
        headerRows = transpose splitCols
    in map concat headerRows

-- Imprime o estado do jogo utilizando as dicas definidas no Game
printGame :: GameState -> IO ()
printGame (GameState currentGrid _ game _) = do
    let gridSize       = length currentGrid
        maxRowHintSize = maximum (map length (rowsHints game))
        paddedRowHints = map (formatHints maxRowHintSize) (rowsHints game)
        colHintsAligned = alignColHints (colsHints game)
        -- Define a margem esquerda para o cabeçalho, alinhada com os row hints e o separador " | "
        leftMargin = replicate (cellWidth * maxRowHintSize + 2) ' '
    
    putStrLn "\nNonogram Grid:\n"
    -- Imprime as linhas do cabeçalho (dicas das colunas) com a margem esquerda
    mapM_ putStrLn $ map (leftMargin ++) colHintsAligned
    -- Imprime cada linha do tabuleiro, precedida pelas dicas das linhas
    mapM_ putStrLn $ zipWith (\rh row -> rh ++ " | " ++ concatMap show row) paddedRowHints currentGrid

-- imprime o menu para o jogador
displayMenu :: IO ()
displayMenu = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1. Marcar célula"
    putStrLn "2. Pedir dica"
    putStrLn "3. Sair"

-- Pega a opção que o jogador decidiu jogar
getUserChoice :: IO Int
getUserChoice = do
    putStr "Opção: "
    choice <- getLine
    return (read choice)

-- Marca uma celula no grid
markCell :: GameState -> IO GameState
markCell gameState = do
    putStrLn "Digite as coordenadas da célula (linha coluna):"
    input <- getLine
    let coords = map read (words input) :: [Int]
    
    if length coords /= 2
        then do
            putStrLn "Entrada inválida. Tente novamente."
            return gameState
        else do
            let (x, y) = (coords !! 0, coords !! 1)
            putStrLn "Digite o tipo de marcação (1 para preenchida, 2 para marcada como incorreta):"
            markType <- getLine
            let cellValue = if markType == "1" then Marked else Filled
            newGameState <- updateCellWithCheck gameState (x, y) cellValue
            
            if lives newGameState < lives gameState
                then putStrLn $ "Jogada errada! Vidas restantes: " ++ show (lives newGameState)
                else putStrLn "Jogada correta!"
            
            return newGameState

-- Da uma dica para o jogador
requestHint :: GameState -> IO GameState
requestHint gameState = do
    newGameState <- giveHint gameState
    return newGameState

-- Roda o jogo
playGame :: GameState -> IO ()
playGame gameState = do
    printGame gameState
    if checkVictory gameState
        then putStrLn "Parabéns! Você venceu o jogo!"
        else if isGameOver gameState
            then putStrLn "Game Over! Você perdeu todas as vidas."
            else do
                displayMenu
                choice <- getUserChoice
                case choice of
                    1 -> do
                        newGameState <- markCell gameState
                        playGame newGameState
                    2 -> do
                        newGameState <- requestHint gameState
                        playGame newGameState
                    3 -> putStrLn "Saindo do jogo..."
                    _ -> do
                        putStrLn "Opção inválida. Tente novamente."
                        playGame gameState


-- Inicia o jogo
startGame :: Game -> IO ()
startGame game = do
    let initialState = initGame game
    playGame initialState