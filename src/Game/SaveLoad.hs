{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Game.SaveLoad
Description : Gerencia o salvamento e carregamento do estado do jogo.

Este módulo lida com a persistência do jogo, permitindo salvar e carregar o estado do jogo
em arquivos JSON. Interage com o sistema de arquivos para ler e escrever os dados do tipo 'GameState'.
-}
module Game.SaveLoad 
    ( saveGame
    , loadGame
    ) where

import Control.Exception (try, SomeException)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import Game.Estrutura (GameState(..))

-- | Diretório base onde os arquivos de save serão armazenados.
baseDir :: FilePath
baseDir = "data/saves"

{-|
Cria o diretório de saves, se não existir.

@return: 'IO ()' – Realiza a criação do diretório, se necessário.
-}
createSaveDirectory :: IO ()
createSaveDirectory = createDirectoryIfMissing True baseDir

{-|
Valida se o estado do jogo é consistente (vidas >= 0).

@param gs: Estado do jogo (GameState).
@return: Booleano que indica se o estado é válido.
-}
validGameState :: GameState -> Bool
validGameState gs = lives gs >= 0

{-|
Salva o estado do jogo em um arquivo JSON.

Escreve o estado do jogo no diretório de saves. Caso ocorra algum erro durante a escrita,
retorna uma mensagem de erro.

@param fileName: Nome do arquivo para salvar o estado.
@param gameState: Estado do jogo a ser salvo.
@return: 'IO (Either String ())' – Retorna 'Right ()' em caso de sucesso, ou 'Left <mensagem>' em caso de erro.
-}
saveGame :: FilePath -> GameState -> IO (Either String ())
saveGame fileName gameState = do
    createSaveDirectory  -- Garante que o diretório exista.
    let fullPath = baseDir ++ "/" ++ fileName
    result <- try (B.writeFile fullPath (encode gameState)) :: IO (Either SomeException ())
    return $ case result of
        Left err -> Left $ "Erro ao salvar jogo: " ++ show err
        Right _  -> Right ()

{-|
Carrega o estado do jogo a partir de um arquivo JSON.

Lê o arquivo especificado e tenta decodificar o conteúdo para o tipo 'GameState'.
Caso o arquivo não exista, ou haja erro na leitura ou decodificação, retorna uma mensagem de erro.

@param fileName: Nome do arquivo de onde carregar o estado do jogo.
@return: 'IO (Either String GameState)' – Retorna o estado carregado ou uma mensagem de erro.
-}
loadGame :: FilePath -> IO (Either String GameState)
loadGame fileName = do
    createSaveDirectory  -- Garante que o diretório exista.
    let fullPath = baseDir ++ "/" ++ fileName
    fileExists <- doesFileExist fullPath
    if not fileExists
        then return $ Left "Arquivo não encontrado."
        else do
            result <- try (B.readFile fullPath) :: IO (Either SomeException B.ByteString)
            case result of
                Left err -> return $ Left ("Erro ao carregar jogo: " ++ show err)
                Right content -> case decode content of
                    Just gs -> if validGameState gs 
                                then return (Right gs)
                                else return (Left "Dados corrompidos ou inconsistentes.")
                    Nothing -> return (Left "Erro: Formato do arquivo inválido.")
