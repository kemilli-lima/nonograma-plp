module Game.UI where

import Game.Logic
import Game.Estrutura
import Data.List (transpose, intersperse)
import System.Console.ANSI
import System.IO

cellWidth :: Int
cellWidth = 2

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

renderCell :: Cell -> String
renderCell Empty  = "\ESC[37m·\ESC[0m "
renderCell Filled = "\ESC[32m■\ESC[0m "
renderCell Marked = "\ESC[31mX\ESC[0m "

drawUI :: GameState -> IO ()
drawUI gameState = do
    clearScreen
    setSGR [SetConsoleIntensity BoldIntensity]
    
    putStrLn $ "\ESC[31mVidas restantes: " ++ intersperse ' ' (replicate (lives gameState) '❤') ++ "\ESC[0m"

    let gameData = game gameState
        current = currentGrid gameState
        maxRowHintSize = maximum (map length (rowsHints gameData))
        paddedRowHints = map (formatHints maxRowHintSize) (rowsHints gameData)
        colHintsAligned = alignColHints (colsHints gameData)
        leftMargin = replicate (cellWidth * maxRowHintSize + 2) ' '

    -- Imprimir dicas das colunas alinhadas com a grade
    mapM_ (putStrLn . (leftMargin ++)) colHintsAligned

    -- Imprimir a grade
    sequence_ $ zipWith (\rh row ->
        putStrLn $  rh ++ " | "++ concatMap renderCell row
        ) paddedRowHints current

-- Menu para o jogador
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
    read <$> getLine

-- Marca uma célula no grid
markCell :: GameState -> IO GameState
markCell gameState = do
    putStrLn "Digite as coordenadas da célula (linha e coluna):"
    input <- getLine
    let coords = map read (words input) :: [Int]

    if length coords /= 2
        then do
            putStrLn "Entrada inválida. Tente novamente."
            return gameState
        else do
            let (x, y) = (coords !! 0, coords !! 1)
            let gridSize = length (currentGrid gameState)
            
            if x < 0 || x >= gridSize || y < 0 || y >= gridSize
                then do
                    putStrLn "Coordenadas fora dos limites do grid. Tente novamente."
                    return gameState
                else do
                    putStrLn "Digite o tipo de marcação (1 para preenchida, 2 para marcada como incorreta):"
                    markType <- getLine
                    let cellValue = if markType == "1" then Marked else Filled
                    newGameState <- updateCellWithCheck gameState (x, y) cellValue
                    
                    if lives newGameState < lives gameState
                        then putStrLn $ "Jogada errada! Vidas restantes: " ++ show (lives newGameState)
                        else putStrLn "Jogada correta!"
                    
                    return newGameState


-- Dá uma dica para o jogador
requestHint :: GameState -> IO GameState
requestHint gameState = do
    newGameState <- giveHint gameState
    return newGameState

-- Roda o jogo
playGame :: GameState -> IO ()
playGame gameState = do
    drawUI gameState  -- exibe o tabuleiro
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

-- Inicia o jogo recebendo também o nome do jogador
startGame :: Game -> String -> IO ()
startGame game name = do
    let initialState = initGame game
    playGame initialState
