{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWithM_" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant return" #-}
{-|
Module      : Game.UI
Description : Implementa a interface do usuário para o jogo.

Este módulo trata da exibição do tabuleiro, dicas, menus e navegação do cursor.
Utiliza funções dos módulos Game.Logic, Game.SaveLoad e Game.Estrutura para atualizar e exibir
o estado do jogo, além de fornecer opções para salvar e interagir com o jogo através do teclado.
-}
module Game.UI where

import Game.Logic
import Game.Estrutura
import Data.List (transpose, intersperse)
import Game.SaveLoad (saveGame)
import System.Console.ANSI
import System.IO
import Control.Monad (when)

-- | Largura fixa para formatação das células.
cellWidth :: Int
cellWidth = 2

{-|
Adiciona espaços à esquerda para alinhar uma string com base em um tamanho fixo.

@param n: Número total de caracteres desejado.
@param s: String original.
@return: String com espaços adicionados à esquerda até atingir o tamanho 'n'.
-}
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

{-|
Formata uma lista de dicas em uma única string.

@param maxSize: Tamanho máximo para alinhamento.
@param hints: Lista de inteiros representando as dicas.
@return: String com as dicas formatadas e alinhadas.
-}
formatHints :: Int -> [Int] -> String
formatHints maxSize hints =
    let hintStrs = map (padLeft cellWidth . show) hints
        padding  = replicate (cellWidth * (maxSize - length hints)) ' '
    in padding ++ concat hintStrs

{-|
Alinha as dicas das colunas para exibição no cabeçalho do tabuleiro.

@param cols: Lista de listas de inteiros, onde cada sublista representa as dicas de uma coluna.
@return: Lista de strings, cada uma representando uma linha do cabeçalho alinhado.
-}
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

{-|
Renderiza uma célula para exibição normal no tabuleiro.

@param cell: Valor do tipo 'Cell' a ser renderizado.
@return: String formatada representando a célula.
-}
renderCell :: Cell -> String
renderCell Empty  = "\ESC[37m·\ESC[0m "
renderCell Filled = "\ESC[32m■\ESC[0m "
renderCell Marked = "\ESC[31mX\ESC[0m "

{-|
Renderiza a célula selecionada com destaque (sublinhado).

@param cell: Valor do tipo 'Cell' que está selecionado.
@return: String formatada com destaque para a célula selecionada.
-}
renderSelectedCell :: Cell -> String
renderSelectedCell cell =
    "\ESC[4m" ++ renderCell cell ++ "\ESC[0m "

{-|
Desenha a interface do jogo, exibindo vidas, dicas e o grid.

@param gameState: Estado atual do jogo.
@return: 'IO ()' – Exibe a interface no terminal.
-}
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
    -- Exibe as dicas das colunas
    mapM_ (putStrLn . (leftMargin ++)) colHintsAligned
    -- Renderiza o grid com destaque na célula selecionada
    let renderedRows = [ paddedRowHints !! i ++ " | " ++ concat [ if (i, j) == (selX, selY)
                                                                   then renderSelectedCell cell
                                                                   else renderCell cell
                                                                 | (j, cell) <- zip [0..] row ]
                       | (i, row) <- zip [0..] current ]
    mapM_ putStrLn renderedRows

{-|
Exibe o menu de opções para o jogador na interface.

@return: 'IO ()' – Exibe o menu principal no terminal.
-}
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

{-|
Loop de navegação que permite ao jogador mover o cursor usando WASD e selecionar uma célula com Enter.

@param gameState: Estado atual do jogo.
@return: 'IO GameState' – Retorna o estado atualizado com a nova posição do cursor.
-}
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
       then return gameState  -- Retorna o estado com o cursor na posição confirmada.
       else navigationLoop updatedState

{-|
Ativa o modo de navegação para que o jogador selecione uma célula para marcar.

Caso a célula selecionada já esteja preenchida, solicita uma nova seleção.
Após a escolha, permite ao jogador definir o tipo de marcação (Filled ou Marked) e atualiza o estado.

@param gameState: Estado atual do jogo.
@return: 'IO GameState' – Retorna o estado do jogo após a marcação.
-}
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
       putStrLn "\nDigite o tipo de marcação para a célula selecionada (1 para preenchida, 2 para marcada não-colorida):"
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

{-|
Solicita uma dica para o jogador, chamando a função 'giveHint' para corrigir uma célula.

@param gameState: Estado atual do jogo.
@return: 'IO GameState' – Retorna o estado do jogo após fornecer a dica.
-}
requestHint :: GameState -> IO GameState
requestHint gameState = do
    newGameState <- giveHint gameState
    return newGameState

{-|
Realiza o salvamento do jogo solicitando o nome do arquivo onde o estado será salvo.

@param gs: Estado atual do jogo.
@return: 'IO GameState' – Retorna o mesmo estado após a operação de salvamento.
-}
saveGamePrompt :: GameState -> IO GameState
saveGamePrompt gs = do
    putStrLn "Digite o nome do save (ex.: save.json):"
    name <- getLine
    result <- saveGame name gs
    case result of
        Left err -> putStrLn ("Erro ao salvar: " ++ err) >> return gs
        Right _  -> putStrLn "Jogo salvo com sucesso!" >> return gs

{-|
Loop principal do jogo, gerenciando a execução com base nas ações do jogador.

Verifica condições de vitória e game over, exibe os menus e processa as escolhas do usuário.

@param gameState: Estado atual do jogo.
@return: 'IO ()' – Executa o loop principal até que o jogador saia ou o jogo termine.
-}
playGame :: GameState -> IO ()
playGame gameState = do
    drawUI gameState  -- Exibe o grid.
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
                  4 -> do
                      newGameState <- saveGamePrompt gameState
                      playGame newGameState
                  _ -> do
                      putStrLn "Opção inválida. Tente novamente."
                      playGame gameState

{-|
Captura e valida a escolha do usuário a partir do menu.

Lê a entrada e verifica se é um número válido entre 1 e 4.

@return: 'IO Int' – Retorna a opção escolhida pelo usuário.
-}
getUserChoice :: IO Int
getUserChoice = do
    setSGR [SetColor Foreground Vivid Cyan]
    putStr "\ESC[36m▶ \ESC[36mOpção: \ESC[0m"
    input <- getLine
    let parsed = reads input :: [(Int, String)]
    case parsed of
        [(n, "")] | n >= 1 && n <= 4 -> return n
        _ -> do
            putStrLn "\ESC[31m❌  Opção inválida! Tente novamente.\ESC[0m"
            getUserChoice

{-|
Inicia o jogo utilizando a estrutura 'Game' e o nome do jogador.

Cria o estado inicial a partir da função 'initGame' e inicia o loop de jogo com 'playGame'.

@param game: Estrutura estática do jogo.
@param name: Nome do jogador.
@return: 'IO ()' – Inicia o jogo.
-}
startGame :: Game -> String -> IO ()
startGame game name = do
    let initialState = initGame game
    playGame initialState
