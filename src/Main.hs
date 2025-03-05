module Main where

import Game.UI (startGame, playGame)
import Game.Estrutura
import Game.PuzzleParser (parsePuzzle)
import Game.SaveLoad (saveGame, loadGame)
import Data.Char (toLower)
import System.IO (stdout, hFlush)
import System.Console.ANSI  -- Importação adicionada
import System.Random (randomRIO)

-- Converte a string de entrada para um nível de dificuldade válido
parseDifficultyInput :: String -> Difficulty
parseDifficultyInput input =
    case map toLower input of
        "1" -> Easy
        "2" -> Medium
        "3" -> Hard
        _   -> Medium -- Padrão caso a entrada seja inválida

chooseRandomGame :: [Game] -> IO Game
chooseRandomGame games = do
    let numGames = length games
    if numGames == 0
        then error "Nenhum jogo disponível no arquivo."
        else do
            index <- randomRIO (0, numGames - 1)
            return $ games !! index

-- Inicia um novo jogo
startNewGame :: String -> IO ()  -- Alterado para receber 'name'
startNewGame name = do
    putStrLn ("\n\ESC[36m Selecione a dificuldade:\ESC[97m\n ")
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
    let difficulty = parseDifficultyInput diff
        puzzlePath = case difficulty of
            Easy   -> "data/puzzles/easy.json"
            Medium -> "data/puzzles/medium.json"
            Hard   -> "data/puzzles/hard.json"
    
    eitherGames <- parsePuzzle puzzlePath
    case eitherGames of
        Left err -> putStrLn $ "Erro: " ++ err
        Right games -> do
            randomGame <- chooseRandomGame games
            startGame randomGame name

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
    putStrLn ("\n\ESC[36mOlá " ++ name ++ "! Escolha uma opção:\ESC[97m\n ")
    putStrLn "╔══════════════════════╗"
    putStrLn "║   \ESC[97m1 ● Novo Jogo \ESC[97m     ║"  -- Verde para fácil
    putStrLn "║   \ESC[97m2 ● Carregar Jogo \ESC[97m ║"  -- Amarelo para médio
    putStrLn "╚══════════════════════╝"
    putStrLn "\ESC[36m ▶ \ESC[36m Opção: \ESC[0m"
    setSGR [Reset]

    -- Lê a escolha do usuário
    diff <- getLine
    hFlush stdout

    case diff of  -- Alterado para usar 'diff' em vez de 'option'
        "1" -> startNewGame name
        "2" -> loadExistingGame
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            main