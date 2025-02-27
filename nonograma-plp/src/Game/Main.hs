module Game.Main where

import Game.UI
import Game.Estrutura

-- Exemplo de estrutura estática do jogo <3
exampleGame :: Game
exampleGame = Game 
    { solution = 
        [ [Marked, Empty,  Empty,  Empty,  Empty,  Empty,  Marked]
        , [Marked, Filled, Filled, Empty, Filled, Filled, Empty]
        , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
        , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
        , [Empty,  Filled, Filled, Filled, Filled, Filled, Empty]
        , [Empty,  Empty,  Filled, Filled, Filled, Empty,  Empty]
        , [Empty,  Empty,  Empty, Filled, Empty, Empty,  Empty]
        ]
    , rowsHints = [[0], [2, 2], [7], [7], [5], [3], [1]]
    , colsHints = [[2], [4], [6], [6], [6], [4], [2]]
    , difficulty = Medium
    }


main :: IO ()
main = startGame exampleGame
