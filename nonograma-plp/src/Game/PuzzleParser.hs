{-# LANGUAGE OverloadedStrings #-}
module Game.PuzzleParser (parsePuzzle) where

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.List (isSuffixOf)
import System.Directory (doesFileExist)
import Game.Estrutura (Game(..), Cell(..), Difficulty(..))

-- | Lê um arquivo de puzzle e o interpreta conforme sua extensão (JSON ou TXT).
parsePuzzle :: FilePath -> IO (Either String Game)
parsePuzzle filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return $ Left "Arquivo não encontrado."
        else do
            content <- B.readFile filePath
            if ".json" `isSuffixOf` filePath
                then parseJSON content
                else parseTXT content

parseJSON :: B.ByteString -> IO (Either String Game)
parseJSON content = case eitherDecode content of
    Left err -> return $ Left $ "Erro JSON: " ++ err
    Right game -> return $ validateGame game

parseTXT :: B.ByteString -> IO (Either String Game)
parseTXT content = do
    let ls = lines (C8.unpack content)
    case parseSections ls of
        Right (sol, rh, ch, diff) -> return $ validateGame (Game sol rh ch diff)
        Left err -> return $ Left err

-- | Divide as seções do arquivo TXT usando linhas em branco como separadores.
parseSections :: [String] -> Either String ([[Cell]], [[Int]], [[Int]], Difficulty)
parseSections ls = do
    (sol, rest1) <- splitAtBlank ls
    (rh, rest2) <- splitAtBlank rest1
    (ch, rest3) <- splitAtBlank rest2
    diffLine <- case rest3 of
        [d] -> parseDifficulty d
        _ -> Left "Formato inválido para dificuldade"
    return (parseSolution sol, parseHints rh, parseHints ch, diffLine)

-- | Separa a lista de linhas na primeira ocorrência de uma linha em branco.
splitAtBlank :: [String] -> Either String ([String], [String])
splitAtBlank [] = Left "Seção faltando"
splitAtBlank xs =
    let (section, rest) = span (/= "") xs
        remaining = dropWhile (== "") rest
    in Right (section, remaining)

-- | Converte as linhas da solução em uma matriz de Cells.
parseSolution :: [String] -> [[Cell]]
parseSolution = map (map readCell . words)

readCell :: String -> Cell
readCell "Filled" = Filled
readCell "Marked" = Marked
readCell _ = Empty

-- | Converte as linhas de dicas em listas de inteiros.
parseHints :: [String] -> [[Int]]
parseHints = map (map read . words)

-- | Converte a string de dificuldade para o tipo Difficulty.
parseDifficulty :: String -> Either String Difficulty
parseDifficulty s = case s of
    "Easy"   -> Right Easy
    "Medium" -> Right Medium
    "Hard"   -> Right Hard
    _        -> Left "Dificuldade inválida"

-- | Valida a integridade do objeto Game.
validateGame :: Game -> Either String Game
validateGame g@(Game sol rh ch _) 
    | length rh /= length sol = Left "Dicas de linhas incompatíveis"
    | null sol || (length ch /= length (head sol)) = Left "Dicas de colunas incompatíveis"
    | otherwise = Right g
