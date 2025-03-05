{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Game.PuzzleParser
Description : Realiza a leitura e interpretação dos puzzles do jogo.
              
Este módulo é responsável por ler arquivos contendo puzzles (em formato JSON ou TXT)
e convertê-los em estruturas do tipo 'Game' definidas no módulo 'Game.Estrutura'. Em arquivos
TXT, o conteúdo é dividido em seções (solução, dicas de linhas, dicas de colunas — números que
indicam a quantidade de células que é preenchida em cada linha/coluna —, e dificuldade), usando
 linhas em branco como separadores. Cada parte é convertida para os tipos correspondentes.
-}
module Game.PuzzleParser (parsePuzzle) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.List (isSuffixOf)
import System.Directory (doesFileExist)
import Game.Estrutura (Game(..), Cell(..), Difficulty(..))

{-|
Lê um arquivo de puzzle e o interpreta conforme sua extensão (JSON ou TXT).

@param filePath: Caminho do arquivo contendo o puzzle.
@return: 'IO (Either String [Game])' – Retorna uma lista de jogos se a leitura e o parsing forem bem-sucedidos, ou uma mensagem de erro.
-}
parsePuzzle :: FilePath -> IO (Either String [Game])
parsePuzzle filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return $ Left "Arquivo não encontrado."
        else do
            content <- B.readFile filePath
            if ".json" `isSuffixOf` filePath
                then parseJSON content
                else do
                    result <- parseTXT content
                    return $ case result of
                        Left err -> Left err
                        Right game -> Right [game]  -- Converte o jogo para uma lista

{-|
Realiza o parsing de um arquivo JSON para extrair uma lista de jogos.

@param content: Conteúdo do arquivo em formato JSON.
@return: 'IO (Either String [Game])' – Retorna a lista de jogos ou uma mensagem de erro caso o parsing falhe.
-}
parseJSON :: B.ByteString -> IO (Either String [Game])
parseJSON content = case eitherDecode content of
    Left err -> return $ Left $ "Erro JSON: " ++ err
    Right games -> return $ Right games

{-|
Realiza o parsing de um arquivo TXT para extrair um jogo.

@param content: Conteúdo do arquivo em formato TXT.
@return: 'IO (Either String Game)' – Retorna o jogo extraído ou uma mensagem de erro caso haja problemas na interpretação.
-}
parseTXT :: B.ByteString -> IO (Either String Game)
parseTXT content = do
    let ls = lines (C8.unpack content)
    case parseSections ls of
        Right (sol, rh, ch, diff) -> return $ validateGame (Game sol rh ch diff)
        Left err -> return $ Left err

{-|
Divide as seções do arquivo TXT utilizando linhas em branco como separadores.

Espera que o arquivo contenha, em ordem:
  1. A solução (grid) – linhas representando as células do puzzle.
  2. Dicas das linhas – cada linha com números que representam os agrupamentos corretos.
  3. Dicas das colunas – dicas para as colunas do puzzle.
  4. Dificuldade – uma única linha indicando o nível (e.g., "Easy", "Medium" ou "Hard").

@param ls: Lista de linhas do arquivo TXT.
@return: 'Either String ([[Cell]], [[Int]], [[Int]], Difficulty)' – Retorna as seções convertidas ou um erro se o formato não corresponder.
-}
parseSections :: [String] -> Either String ([[Cell]], [[Int]], [[Int]], Difficulty)
parseSections ls = do
    (sol, rest1) <- splitAtBlank ls
    (rh, rest2) <- splitAtBlank rest1
    (ch, rest3) <- splitAtBlank rest2
    diffLine <- case rest3 of
        [d] -> parseDifficulty d
        _ -> Left "Formato inválido para dificuldade"
    return (parseSolution sol, parseHints rh, parseHints ch, diffLine)

{-|
Separa a lista de linhas na primeira ocorrência de uma linha em branco.

@param xs: Lista de linhas.
@return: 'Either String ([String], [String])' – Retorna a seção extraída e as linhas restantes, ou um erro se não for possível dividir.
-}
splitAtBlank :: [String] -> Either String ([String], [String])
splitAtBlank [] = Left "Seção faltando"
splitAtBlank xs =
    let (section, rest) = span (/= "") xs
        remaining = dropWhile (== "") rest
    in Right (section, remaining)

{-|
Converte as linhas da solução em uma matriz de 'Cell'.

Cada linha do arquivo é dividida em palavras, e cada palavra é convertida para o tipo 'Cell'
usando a função 'readCell'.

@param: Lista de strings representando as linhas da solução.
@return: Uma matriz (lista de listas) de 'Cell'.
-}
parseSolution :: [String] -> [[Cell]]
parseSolution = map (map readCell . words)

{-|
Converte uma string em um valor do tipo 'Cell'.

Mapeia strings específicas para os construtores correspondentes:
  - "Filled" para 'Filled'
  - "Marked" para 'Marked'
  - Qualquer outro valor resulta em 'Empty'

@param: String representando o estado da célula.
@return: Valor do tipo 'Cell'.
-}
readCell :: String -> Cell
readCell "Filled" = Filled
readCell "Marked" = Marked
readCell _        = Empty

{-|
Converte as linhas de dicas em listas de inteiros.

Cada linha é dividida em palavras, e cada palavra é convertida para 'Int'.

@param: Lista de strings representando as linhas de dicas.
@return: Lista de listas de inteiros com as dicas.
-}
parseHints :: [String] -> [[Int]]
parseHints = map (map read . words)

{-|
Converte a string de dificuldade para o tipo 'Difficulty'.

Aceita apenas as strings "Easy", "Medium" ou "Hard". Caso contrário, retorna um erro.

@param s: String representando a dificuldade.
@return: 'Either String Difficulty' – Dificuldade convertida ou erro se inválida.
-}
parseDifficulty :: String -> Either String Difficulty
parseDifficulty s = case s of
    "Easy"   -> Right Easy
    "Medium" -> Right Medium
    "Hard"   -> Right Hard
    _        -> Left "Dificuldade inválida"

{-|
Valida a integridade do objeto 'Game'. Ou seja, se o número de dicas do grid corresponde à quantidade de linhas/colunas dele.

@param g: Objeto 'Game' a ser validado, contendo a solução, dicas de linhas, dicas de colunas e dificuldade.
@return: 'Either String Game' – Retorna o jogo se válido ou uma mensagem de erro explicando a inconsistência.
-}
validateGame :: Game -> Either String Game
validateGame g@(Game sol rh ch _) 
    | length rh /= length sol = Left "Dicas de linhas incompatíveis: o número de dicas não corresponde ao número de linhas da solução."
    | null sol || (length ch /= length (head sol)) = Left "Dicas de colunas incompatíveis: a solução está vazia ou o número de dicas não corresponde ao número de colunas da solução."
    | otherwise = Right g
