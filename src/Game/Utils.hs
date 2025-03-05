-- src/Utils.hs
module Game.Utils
    ( isValidCoordinate
    , formatHints
    ) where

-- | Valida se as coordenadas (x, y) estão dentro dos limites do tabuleiro.
-- O primeiro argumento é uma tupla com (número de linhas, número de colunas).
isValidCoordinate :: (Int, Int) -> (Int, Int) -> Bool
isValidCoordinate (maxRow, maxCol) (x, y) = 
    x >= 0 && x < maxRow && y >= 0 && y < maxCol



-- | Formata uma lista de dicas para exibição alinhada.
-- cellWidth: largura de cada célula (ex.: 2).
-- maxSize: número máximo de dicas para padronizar o tamanho.
-- hints: lista de números (dicas).
formatHints :: Int -> Int -> [Int] -> String
formatHints cellWidth maxSize hints =
    let padLeft n s = replicate (n - length s) ' ' ++ s
        hintStrs = map (padLeft cellWidth . show) hints
        padding  = replicate (cellWidth * (maxSize - length hints)) ' '
    in padding ++ concat hintStrs
