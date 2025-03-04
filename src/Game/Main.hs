module Game.Main where

import Game.UI
import Game.Estrutura
import Data.Char (toLower)
import System.IO (stdout, hFlush)
import System.Console.ANSI
    ( setSGR, ConsoleIntensity(BoldIntensity), SGR(Reset, SetConsoleIntensity) )

-- Definindo os jogos para cada nível de dificuldade
easyGame :: Game
easyGame = Game 
    [ [Filled, Filled, Filled, Marked]
    , [Marked, Filled, Filled, Filled]
    , [Filled, Marked, Filled, Marked]
    , [Filled, Filled, Marked, Filled]
    ] [[3], [3], [2], [3]] [[2], [2], [3], [3]] Easy

mediumGame :: Game
mediumGame = Game 
    [ [Marked, Marked, Filled, Marked, Marked]  
    , [Marked, Filled, Filled, Filled, Marked]
    , [Filled, Filled, Filled, Filled, Filled]
    , [Marked, Filled, Filled, Filled, Marked]
    , [Marked, Marked, Filled, Marked, Marked] 
    ] [[1], [3], [5], [3], [1]] [[1], [3], [5], [3], [1]] Medium

hardGame :: Game
hardGame = Game 
    [ [Marked, Empty, Empty, Empty, Empty, Empty, Marked]
    , [Marked, Filled, Filled, Empty, Filled, Filled, Empty]
    , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
    , [Filled, Filled, Filled, Filled, Filled, Filled, Filled]
    , [Empty, Filled, Filled, Filled, Filled, Filled, Empty]
    , [Empty, Empty, Filled, Filled, Filled, Empty, Empty]
    , [Empty, Empty, Empty, Filled, Empty, Empty, Empty]
    ] [[0], [2, 2], [7], [7], [5], [3], [1]] [[2], [4], [6], [6], [6], [4], [2]] Hard

-- Converte a string de entrada para um nível de dificuldade válido
parseDifficulty :: String -> Difficulty
parseDifficulty input =
    case map toLower input of
        "1"   -> Easy
        "2"   -> Medium
        "3"   -> Hard
        _     -> Medium  -- Padrão caso a entrada seja inválida

main :: IO ()
main = do
    hFlush stdout
    putStrLn "\ESC[36m██  █  ███  ██  █  ███  █████  ███  ████ █   █ ████ "
    putStrLn "\ESC[36m█ █ █ █   █ █ █ █ █   █ █      █  █ █  █ ██ ██ █  █ "
    putStrLn "\ESC[36m█  ██ █   █ █  ██ █   █ █ ███  ███  ████ █ █ █ ████  "
    putStrLn "\ESC[36m█   █  ███  █   █  ███  █████  █  █ █  █ █   █ █  █  "
    setSGR [SetConsoleIntensity BoldIntensity]
    putStrLn "\n\ESC[36mBem-vindo(a) ao jogo! Qual o seu nome?\ESC[97m\n"
    name <- getLine
    putStrLn ("\n\ESC[36mOlá " ++ name ++ "! Selecione a dificuldade:\ESC[97m\n ")
    putStrLn "╔══════════════════════╗"
    putStrLn "║   \ESC[32m1 ● Fácil \ESC[97m         ║"  -- Verde para fácil
    putStrLn "║   \ESC[33m2 ● Médio \ESC[97m         ║"  -- Amarelo para médio
    putStrLn "║   \ESC[31m3 ● Difícil \ESC[97m       ║"  -- Vermelho para difícil
    putStrLn "╚══════════════════════╝"
    putStrLn "\ESC[36m ▶ \ESC[36m Opção: \ESC[0m"
    setSGR [Reset]

    -- Lê a escolha do usuário
    diff <- getLine
    hFlush stdout
    
    -- Determina a dificuldade escolhida
    let difficulty = parseDifficulty diff
        selectedGame = case difficulty of
            Easy   -> easyGame
            Medium -> mediumGame
            Hard   -> hardGame
    
    -- Inicia o jogo com a dificuldade selecionada
    startGame selectedGame name
