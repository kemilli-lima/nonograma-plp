module Main where

import Game.UI (startGame, playGame)
import Game.Estrutura
import Game.PuzzleParser (parsePuzzle)
import Game.SaveLoad (saveGame, loadGame)
import Data.Char (toLower)
import System.IO (stdout, hFlush)

easyGame :: Game
easyGame = Game 
    [ [Filled, Filled, Filled, Marked]
    , [Marked, Filled, Filled, Filled]
    , [Filled, Marked, Filled, Marked]
    , [Filled, Filled, Marked, Filled]
    ] [[3],[3],[2],[3]] [[2],[2],[3],[3]] Easy
    
mediumGame :: Game
mediumGame = Game 
    [ [Marked, Marked, Filled, Marked, Marked]  
    , [Marked, Filled, Filled, Filled, Marked]
    , [Filled, Filled, Filled, Filled, Filled]
    , [Marked, Filled, Filled, Filled, Marked]
    , [Marked, Marked, Filled, Marked, Marked] 
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
parseDifficultyInput :: String -> Difficulty
parseDifficultyInput input =
    case map toLower input of
        "f" -> Easy
        "m" -> Medium
        "d" -> Hard
        _   -> Medium -- Padrão caso a entrada seja inválida

main :: IO ()
main = do
    hFlush stdout
    putStrLn "\ESC[36m\nBem-vindo ao jogo Nonograma! \ESC[0m"
    putStrLn "\ESC[36m-----------------------------------------\ESC[0m"
    
    -- Menu principal
    putStrLn "Escolha uma opção:"
    putStrLn "1. Novo Jogo"
    putStrLn "2. Carregar Jogo"
    option <- getLine
    
    case option of
        "1" -> startNewGame
        "2" -> loadExistingGame
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            main

-- Inicia um novo jogo
startNewGame :: IO ()
startNewGame = do
    putStrLn "Digite seu nome:"
    name <- getLine
    putStrLn "\n"
    putStrLn (name ++ ", selecione a dificuldade que você deseja (F para Fácil/M para Médio/D para Difícil):")
    diff <- getLine
    hFlush stdout
    let difficulty = parseDifficultyInput diff
        puzzlePath = case difficulty of
            Easy   -> "data/puzzles/easy.txt"
            Medium -> "data/puzzles/medium.txt"
            Hard   -> "data/puzzles/hard.txt"
    eitherGame <- parsePuzzle puzzlePath
    case eitherGame of
        Left err -> putStrLn $ "Erro: " ++ err
        Right game -> startGame game name


-- Carrega um jogo existente
loadExistingGame :: IO ()
loadExistingGame = do
    putStrLn "Digite o nome do save:"
    saveName <- getLine
    result <- loadGame saveName
    case result of
        Left err -> do
            putStrLn $ "Erro ao carregar jogo: " ++ err
            main
        Right gameState -> do
            putStrLn $ "Jogo carregado com sucesso!"
            playGame gameState