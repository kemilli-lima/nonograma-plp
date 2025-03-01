module Game.Main where

import Game.UI (startGame)
import Game.Estrutura
import Data.Char (toLower)
import System.IO (stdout, hFlush)

easyGame :: Game
easyGame = Game 
    [ [Filled, Filled, Filled, Empty]
    , [Empty, Filled, Filled, Filled]
    , [Filled, Empty, Filled, Empty]
    , [Filled, Filled, Empty, Filled]
    ] [[3],[3],[2],[3]] [[2],[2],[3],[3]] Easy
    
mediumGame :: Game
mediumGame = Game 
    [ [Empty, Empty, Filled, Empty, Empty]  
    , [Empty, Filled, Filled, Filled, Empty]
    , [Filled, Filled, Filled, Filled, Filled]
    , [Empty, Filled, Filled, Filled, Empty]
    , [Empty, Empty, Filled, Empty, Empty] 
    ] [[1], [3], [5], [3], [1]] [[1], [3], [5], [3], [1]]
    Medium


hardGame :: Game
hardGame = Game 
    [[Marked, Empty,  Empty,  Empty,  Empty,  Empty,  Marked]
        , [Marked, Filled, Filled, Empty, Filled, Filled, Empty]
        , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
        , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
        , [Empty,  Filled, Filled, Filled, Filled, Filled, Empty]
        , [Empty,  Empty,  Filled, Filled, Filled, Empty,  Empty]
        , [Empty,  Empty,  Empty, Filled, Empty, Empty,  Empty]
        ]
    [[0], [2, 2], [7], [7], [5], [3], [1]] [[2], [4], [6], [6], [6], [4], [2]] Hard


-- Converte a string de entrada para um nível de dificuldade válido
parseDifficulty :: String -> Difficulty
parseDifficulty input =
    case map toLower input of
        "f"   -> Easy
        "m"   -> Medium
        "d" -> Hard
        _         -> Medium  -- Padrão caso a entrada seja inválida

main :: IO ()
main = do
    hFlush stdout
    putStrLn "\ESC[36m\nBem-vindo ao jogo Nonograma! \ESC[0m"
    putStrLn "\ESC[36m-----------------------------------------\ESC[0m"
    putStrLn "Digite seu nome:"
    name <- getLine
    putStrLn "\n"
    putStrLn (name ++ ", selecione a dificuldade que você deseja (F para Fácil/M para Médio/D para Difícil):")
    diff <- getLine
    hFlush stdout
    
    let difficulty = parseDifficulty diff
        selectedGame = case difficulty of
            Easy   -> easyGame
            Medium -> mediumGame
            Hard   -> hardGame
    
    startGame selectedGame name
