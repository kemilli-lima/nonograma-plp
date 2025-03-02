module Game.FileIO (saveGame, loadGame) where

import System.IO (IOMode(..))
import Control.Exception (try, SomeException)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Game.Estrutura (GameState(..))  -- Importa a estrutura do jogo

-- Salva o estado do jogo em um arquivo JSON
saveGame :: FilePath -> GameState -> IO (Either String ())
saveGame filePath gameState = do
    let dir = "nonograma-plp/data/saves"
        fullPath = dir ++ "/" ++ filePath
    createDirectoryIfMissing True dir  -- Cria diretório se não existir
    result <- try (B.writeFile fullPath (encode gameState)) :: IO (Either SomeException ())
    return $ case result of
        Left err  -> Left $ "Erro ao salvar jogo: " ++ show err
        Right _   -> Right ()

-- Carrega o estado do jogo a partir de um arquivo JSON
loadGame :: FilePath -> IO (Either String GameState)
loadGame filePath = do
    let fullPath = "nonograma-plp/data/saves/" ++ filePath
    fileExists <- doesFileExist fullPath
    if not fileExists
        then return $ Left "Arquivo não encontrado."
        else do
            result <- try (B.readFile fullPath) :: IO (Either SomeException B.ByteString)
            case result of
                Left err  -> return $ Left ("Erro ao carregar jogo: " ++ show err)
                Right content -> case decode content of
                    Just gs -> if validGameState gs 
                                then return (Right gs)
                                else return (Left "Dados corrompidos ou inconsistentes.")
                    Nothing -> return (Left "Erro: Formato do arquivo inválido.")

-- Verifica se o estado do jogo é válido
validGameState :: GameState -> Bool
validGameState gs = lives gs >= 0  
