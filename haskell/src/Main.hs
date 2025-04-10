{-|
Module      : Main
Description : Ponto de entrada da aplicação.

Integra os módulos Game.UI, Game.Estrutura, Game.PuzzleParser e Game.SaveLoad para oferecer
ao usuário a opção de iniciar um novo jogo ou carregar um jogo salvo.
-}
module Main where

import Game.UI (startGame, playGame)
import Game.Estrutura
import Game.PuzzleParser (parsePuzzle)
import Game.SaveLoad (saveGame, loadGame)
import Data.Char (toLower)
import System.IO (stdout, hFlush)
import System.Console.ANSI
import System.Random (randomRIO)

{-|
Converte a entrada do usuário para um nível de dificuldade válido.

@param input: String fornecida pelo usuário.
@return: Valor do tipo 'Difficulty' (Easy, Medium ou Hard).
-}
parseDifficultyInput :: String -> Difficulty
parseDifficultyInput input =
    case map toLower input of
        "1" -> Easy
        "2" -> Medium
        "3" -> Hard
        _   -> Medium -- Padrão para entrada inválida

{-|
Seleciona aleatoriamente um jogo a partir de uma lista de jogos.

@param games: Lista de jogos disponíveis.
@return: 'IO Game' – Retorna um jogo escolhido aleatoriamente.
-}
chooseRandomGame :: [Game] -> IO Game
chooseRandomGame games = do
    let numGames = length games
    if numGames == 0
        then error "Nenhum jogo disponível no arquivo."
        else do
            index <- randomRIO (0, numGames - 1)
            return $ games !! index

{-|
Inicia um novo jogo solicitando a dificuldade e carregando o puzzle correspondente.

Exibe opções de dificuldade, lê a escolha do usuário e inicia o jogo com base no puzzle selecionado.

@param name: Nome do jogador.
@return: 'IO ()' – Inicia o fluxo do novo jogo.
-}
startNewGame :: String -> IO ()
startNewGame name = do
    putStrLn ("\n\ESC[36m Selecione a dificuldade:\ESC[97m\n ")
    putStrLn "╔══════════════════════╗"
    putStrLn "║   \ESC[32m1 ● Fácil \ESC[97m         ║"
    putStrLn "║   \ESC[33m2 ● Médio \ESC[97m         ║"
    putStrLn "║   \ESC[31m3 ● Difícil \ESC[97m       ║"
    putStrLn "╚══════════════════════╝"
    putStrLn "\ESC[36m ▶ \ESC[36m Opção: \ESC[0m"
    setSGR [Reset]
    diff <- getLine
    hFlush stdout
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

{-|
Carrega um jogo salvo a partir de um arquivo.

Lê o nome do save, tenta carregar o jogo e, em caso de sucesso, inicia o loop do jogo.

@return: 'IO ()' – Inicia o jogo com o estado carregado ou retorna ao menu em caso de erro.
-}
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

{-|
Função principal que inicia a aplicação.

Exibe uma mensagem de boas-vindas, solicita o nome do jogador e apresenta opções para
iniciar um novo jogo ou carregar um jogo salvo.

@return: 'IO ()' – Inicia a aplicação e mantém o loop principal.
-}
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
    putStrLn "║   \ESC[97m1 ● Novo Jogo \ESC[97m     ║"
    putStrLn "║   \ESC[97m2 ● Carregar Jogo \ESC[97m ║"
    putStrLn "╚══════════════════════╝"
    putStrLn "\ESC[36m ▶ \ESC[36m Opção: \ESC[0m"
    setSGR [Reset]
    diff <- getLine
    hFlush stdout
    case diff of
        "1" -> startNewGame name
        "2" -> loadExistingGame
        _   -> do
            putStrLn "Opção inválida! Tente novamente."
            main
