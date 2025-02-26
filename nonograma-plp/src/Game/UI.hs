module Game.UI where

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
