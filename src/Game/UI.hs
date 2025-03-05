{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWithM_" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant return" #-}
module Game.UI where

import Game.Logic
import Game.Estrutura
import Data.List (transpose, intersperse)
import Game.SaveLoad (saveGame)
import System.Console.ANSI
import System.IO
import Control.Monad (when)

cellWidth :: Int
cellWidth = 2

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Formata uma lista de dicas em uma string com largura fixa
formatHints :: Int -> [Int] -> String
formatHints maxSize hints =
    let hintStrs = map (padLeft cellWidth . show) hints
        padding  = replicate (cellWidth * (maxSize - length hints)) ' '
    in padding ++ concat hintStrs

-- Alinha as dicas das colunas (cabeçalho)
alignColHints :: [[Int]] -> [String]
alignColHints cols =
    let maxHintSize = maximum (map length cols)
        paddedCols = map (formatHints maxHintSize) cols
        splitCol :: String -> [String]
        splitCol [] = []
        splitCol s  = let (chunk, rest) = splitAt cellWidth s
                      in chunk : splitCol rest
        splitCols = map splitCol paddedCols
        headerRows = transpose splitCols
    in map concat headerRows

-- Renderiza uma célula normalmente
renderCell :: Cell -> String
renderCell Empty  = "\ESC[37m·\ESC[0m "
renderCell Filled = "\ESC[32m■\ESC[0m "
renderCell Marked = "\ESC[31mX\ESC[0m "

-- Renderiza a célula selecionada com realce usando sublinhado
renderSelectedCell :: Cell -> String
renderSelectedCell cell =
    "\ESC[4m" ++ renderCell cell ++ "\ESC[0m "

-- Desenha a interface do jogo, destacando a célula selecionada
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
        (selX, selY) = selectedCell gameState
    -- Imprime as dicas das colunas
    mapM_ (putStrLn . (leftMargin ++)) colHintsAligned
    -- Renderiza o grid com realce para a célula selecionada
    let renderedRows = [ paddedRowHints !! i ++ " | " ++ concat [ if (i, j) == (selX, selY)
                                                                   then renderSelectedCell cell
                                                                   else renderCell cell
                                                                 | (j, cell) <- zip [0..] row ]
                       | (i, row) <- zip [0..] current ]
    mapM_ putStrLn renderedRows


-- Menu para o jogador
displayMenu :: IO ()
displayMenu = do
    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "\n╔════════════════════════════════╗"
    putStrLn "║    🎮 ESCOLHA UMA OPÇÃO 🎮     ║"
    putStrLn "╠════════════════════════════════╣"
    
    setSGR [Reset]
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "║ 1. ✏️ Marcar célula (via WASD)  ║"
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Yellow]
    putStrLn "║ 2. 💡 Pedir dica               ║"
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Magenta]
    putStrLn "║ 3. 🚪 Sair                     ║"
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Green]
    putStrLn "║ 4. 💾 Salvar jogo              ║"
    setSGR [Reset]

    setSGR [SetColor Foreground Vivid Cyan]
    putStrLn "╚════════════════════════════════╝"
    setSGR [Reset]
    putStrLn ""

-- Loop de navegação: atualiza a posição do cursor com base na tecla pressionada
navigationLoop :: GameState -> IO GameState
navigationLoop gameState = do
    drawUI gameState
    putStrLn "\ESC[36mUse WASD para mover o cursor, Enter para selecionar a célula.\ESC[0m"
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    key <- getChar
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    let (x, y) = selectedCell gameState
        grid = currentGrid gameState
        numRows = length grid
        numCols = length (head grid)
        newPos = case key of
                    'w' -> (max 0 (x - 1), y)
                    's' -> (min (numRows - 1) (x + 1), y)
                    'a' -> (x, max 0 (y - 1))
                    'd' -> (x, min (numCols - 1) (y + 1))
                    _   -> (x, y)
        updatedState = gameState { selectedCell = newPos }
    if key == '\n'
       then return gameState  -- Confirma a posição atual e encerra a navegação
       else navigationLoop updatedState

-- Ativa o modo de navegação para marcar a célula selecionada via WASD
navigateAndMark :: GameState -> IO GameState
navigateAndMark gameState = do
    putStrLn "\ESC[36mNavegação ativada: mova o cursor para selecionar a célula.\ESC[0m"
    newState <- navigationLoop gameState
    let (selX, selY) = selectedCell newState
        cell = (currentGrid newState) !! selX !! selY
    if cell /= Empty then do
       putStrLn "\ESC[31mEsta célula já está pintada. Selecione outra célula.\ESC[0m"
       hFlush stdout
       putStrLn "Pressione ENTER para continuar..."
       _ <- getLine
       navigateAndMark newState
    else do
       putStrLn "\nDigite o tipo de marcação para a célula selecionada (1 para preenchida, 2 para marcada incorreta):"
       markType <- getLine
       let cellValue = if markType == "1" then Filled else Marked
       updatedGameState <- updateCellWithCheck newState (selX, selY) cellValue
       if lives updatedGameState < lives newState then
           putStrLn $ "Vidas restantes: " ++ show (lives updatedGameState)
       else
           putStrLn "Jogada correta!"
       hFlush stdout
       putStrLn "Pressione ENTER para continuar..."
       _ <- getLine
       return updatedGameState


-- Dá uma dica para o jogador
requestHint :: GameState -> IO GameState
requestHint gameState = do
    newGameState <- giveHint gameState
    return newGameState

-- Realiza o salvamento do jogo usando o módulo SaveLoad unificado
saveGamePrompt :: GameState -> IO GameState
saveGamePrompt gs = do
    putStrLn "Digite o nome do save (ex.: save.json):"
    name <- getLine
    result <- saveGame name gs
    case result of
        Left err -> putStrLn ("Erro ao salvar: " ++ err) >> return gs
        Right _  -> putStrLn "Jogo salvo com sucesso!" >> return gs

-- Loop principal do jogo com mensagens de vitória e game over decoradas
playGame :: GameState -> IO ()
playGame gameState = do
    drawUI gameState  -- exibe o tabuleiro
    if checkVictory gameState
       then do
           setSGR [SetColor Foreground Vivid Green]
           setSGR [SetConsoleIntensity BoldIntensity]
           putStrLn "Parabéns! Você venceu o jogo! 🎉"
           setSGR [Reset]
       else if isGameOver gameState
           then do
               setSGR [SetColor Foreground Vivid Red]
               setSGR [SetConsoleIntensity BoldIntensity]
               putStrLn "💀 Game Over! Você perdeu todas as vidas. Tente novamente! 💀"
               setSGR [Reset]
           else do
              displayMenu
              putStrLn "\ESC[36mEscolha uma opção: \ESC[0m"
              choice <- getUserChoice
              case choice of
                  1 -> do
                      newGameState <- navigateAndMark gameState
                      playGame newGameState
                  2 -> do
                      newGameState <- requestHint gameState
                      playGame newGameState
                  3 -> putStrLn "Saindo do jogo..."
                  _ -> do
                      putStrLn "Opção inválida. Tente novamente."
                      playGame gameState

-- Inicia o jogo, recebendo também o nome do jogador
startGame :: Game -> String -> IO ()
startGame game name = do
    let initialState = initGame game
    playGame initialState

-- Captura a opção do usuário (sem alterações significativas)
getUserChoice :: IO Int
getUserChoice = do
    setSGR [SetColor Foreground Vivid Cyan]
    putStr "\ESC[36m▶ \ESC[36mOpção: \ESC[0m"
    input <- getLine
    let parsed = reads input :: [(Int, String)]
    case parsed of
        [(n, "")] | n >= 1 && n <= 3 -> return n
        _ -> do
            putStrLn "\ESC[31m❌  Opção inválida! Tente novamente.\ESC[0m"
            getUserChoice