module Game.Logic
    ( updateCellWithCheck
    , checkVictory
    , giveHint
    , isGameOver
    ) where

import Game.Estrutura

-- Atualiza uma célula do tabuleiro e verifica se a marcação está correta
updateCellWithCheck :: GameState -> (Int, Int) -> Cell -> IO GameState
updateCellWithCheck gameState (x, y) cellValue
    | x < 0 || x >= length grid || y < 0 || y >= length (head grid) = do
        -- Coordenadas fora dos limites: retorna o estado inalterado
        putStrLn "Coordenadas inválidas! Jogada ignorada."
        return gameState
    | cellValue /= correctValue = do
        -- Jogada incorreta: reduz as vidas e mantém o grid inalterado
        putStrLn "Jogada errada! Você perdeu uma vida."
        return gameState { lives = max 0 (lives gameState - 1) }
    | otherwise = do
        -- Jogada correta: atualiza o grid e verifica se o jogo foi resolvido
        let newGrid = updateGrid grid (x, y) cellValue
            newSolved = newGrid == sol
        return gameState { currentGrid = newGrid, isSolved = newSolved }
  where
    grid = currentGrid gameState
    sol = solution (game gameState)
    correctValue = (sol !! x) !! y

-- Função auxiliar para atualizar o grid
updateGrid :: [[Cell]] -> (Int, Int) -> Cell -> [[Cell]]
updateGrid grid (x, y) cellValue =
    take x grid ++
    [take y (grid !! x) ++ [cellValue] ++ drop (y + 1) (grid !! x)] ++
    drop (x + 1) grid

-- Verifica se o jogador venceu o jogo
checkVictory :: GameState -> Bool
checkVictory gameState = currentGrid gameState == solution (game gameState)

-- Fornece uma dica ao jogador marcando corretamente uma célula
giveHint :: GameState -> IO GameState
giveHint gameState = do
    let grid = currentGrid gameState
        sol = solution (game gameState)
        hintCell = [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1], (grid !! x) !! y /= (sol !! x) !! y]
    case hintCell of
        [] -> return gameState
        ((x, y):_) -> updateCellWithCheck gameState (x, y) (sol !! x !! y)

-- Verifica se o jogo acabou por falta de vidas
isGameOver :: GameState -> Bool
isGameOver gameState = lives gameState <= 0
