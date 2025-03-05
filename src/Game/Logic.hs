module Game.Logic
    ( updateCellWithCheck
    , checkVictory
    , giveHint
    , isGameOver
    ) where

import Game.Estrutura
import Game.Utils (isValidCoordinate)

isCorrectMove :: GameState -> (Int, Int) -> Cell -> Bool
isCorrectMove gameState (x, y) value =
    let correctValue = solution (game gameState) !! x !! y
    in value == correctValue

updateCellWithCheck :: GameState -> (Int, Int) -> Cell -> IO GameState
updateCellWithCheck gameState (x, y) cellValue
    | not (isValidCoordinate (rows, cols) (x, y)) = do
        putStrLn "Coordenadas inválidas!"
        return gameState
    | otherwise = do
        let correctValue = solution (game gameState) !! x !! y
        let oldCell = (currentGrid gameState !! x) !! y
        
        if oldCell == cellValue then do
            putStrLn "A célula já está com este valor."
            return gameState
        else if cellValue /= correctValue then do
            putStrLn "Jogada errada! Você perdeu uma vida."
            return gameState { lives = max 0 (lives gameState - 1) }
        else do
            -- Jogada correta: atualiza a grade sem perder vidas
            let newGrid = updateGrid (currentGrid gameState) (x, y) cellValue
            return gameState { currentGrid = newGrid }
  where
    grid = currentGrid gameState
    rows = length grid
    cols = if null grid then 0 else length (head grid)

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
