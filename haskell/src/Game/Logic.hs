{-|
Module      : Game.Logic
Description : Implementa as regras e a lógica do jogo.
              
Este módulo gerencia as operações de atualização do grid, validação de jogadas, fornecimento
de dicas e verificação das condições de vitória ou término do jogo. Interage com 'Game.Estrutura'
para acessar os dados do jogo e com 'Game.Utils' para validar coordenadas.
-}
module Game.Logic
    ( updateCellWithCheck
    , checkVictory
    , giveHint
    , isGameOver
    ) where

import Game.Estrutura
import Game.Utils (isValidCoordinate)

-- | Verifica se a jogada é correta, comparando o valor inserido com o valor correto na solução.
--
-- @param gameState: Estado atual do jogo.
-- @param (x, y): Coordenadas da célula a ser atualizada.
-- @param value: Valor que o jogador deseja inserir.
-- @return: Booleano que indica se o valor inserido é igual ao valor correto.
isCorrectMove :: GameState -> (Int, Int) -> Cell -> Bool
isCorrectMove gameState (x, y) value =
    let correctValue = solution (game gameState) !! x !! y
    in value == correctValue

{-|
Atualiza uma célula do grid após realizar as verificações necessárias.

Verifica se as coordenadas são válidas, se a célula já contém o valor desejado, e se o valor
inserido está correto. Se a jogada estiver errada, deduz uma vida; se estiver correta, atualiza o grid.

@param gameState: Estado atual do jogo.
@param (x, y): Coordenadas da célula a ser atualizada.
@param cellValue: Novo valor da célula.
@return: 'IO GameState' com o estado atualizado.
-}
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
            -- Jogada correta: atualiza o grid.
            let newGrid = updateGrid (currentGrid gameState) (x, y) cellValue
            return gameState { currentGrid = newGrid }
  where
    grid = currentGrid gameState
    rows = length grid
    cols = if null grid then 0 else length (head grid)

{-|
Atualiza o grid substituindo o valor da célula na posição especificada.

@param grid: Grid atual do jogo.
@param (x, y): Coordenadas da célula a ser atualizada.
@param cellValue: Novo valor para a célula.
@return: Novo grid com a célula atualizada.
-}
updateGrid :: [[Cell]] -> (Int, Int) -> Cell -> [[Cell]]
updateGrid grid (x, y) cellValue =
    take x grid ++
    [take y (grid !! x) ++ [cellValue] ++ drop (y + 1) (grid !! x)] ++
    drop (x + 1) grid

{-|
Verifica se o jogador completou o puzzle corretamente.

Compara o grid atual com a solução.

@param gameState: Estado atual do jogo.
@return: Booleano que indica se o grid atual é igual à solução.
-}
checkVictory :: GameState -> Bool
checkVictory gameState = currentGrid gameState == solution (game gameState)

{-|
Fornece uma dica ao jogador, corrigindo a primeira célula em que o grid atual diverge da solução.

@param gameState: Estado atual do jogo.
@return: 'IO GameState' com o grid atualizado após a dica.
-}
giveHint :: GameState -> IO GameState
giveHint gameState = do
    let grid = currentGrid gameState
        sol  = solution (game gameState)
        hintCell = [ (x, y)
                   | x <- [0 .. length grid - 1]
                   , y <- [0 .. length (head grid) - 1]
                   , (grid !! x) !! y /= (sol !! x) !! y
                   ]
    case hintCell of
        []         -> return gameState
        ((x, y):_) -> updateCellWithCheck gameState (x, y) (sol !! x !! y)

{-|
Verifica se o jogo acabou por falta de vidas.

@param gameState: Estado atual do jogo.
@return: Booleano que indica se as vidas restantes são zero ou menos.
-}
isGameOver :: GameState -> Bool
isGameOver gameState = lives gameState <= 0

