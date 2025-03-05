{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}

{-|
Module      : Game.Estrutura
Description : Define as estruturas básicas e o estado inicial do jogo.
              
Este módulo define os tipos fundamentais que representam o estado e a estrutura do jogo,
incluindo as células, o grid, os níveis de dificuldade, a estrutura estática do jogo (Game) 
e o estado dinâmico (GameState). Outros módulos, como Game.Logic, Game.PuzzleParser, 
Game.SaveLoad e Game.UI, utilizam essas definições.
-}
module Game.Estrutura
    ( Cell(..)
    , Grid
    , Difficulty(..)
    , Game(..)
    , GameState(..)
    , initGame
    ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Representa os possíveis estados de uma célula.
data Cell = Empty   -- ^ Célula sem decisão.
          | Filled  -- ^ Célula marcada como preenchida.
          | Marked  -- ^ Célula marcada como não-preenchida.
          deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Cell where
    show Empty  = ". "  
    show Filled = "x "  
    show Marked = "* "  

-- | Define um grid como uma lista de listas de células.
type Grid = [[Cell]]

-- | Representa os níveis de dificuldade do jogo.
data Difficulty = Easy   
                | Medium 
                | Hard   
                deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
Representa a estrutura estática do jogo.

Contém a solução final do puzzle, dicas para as linhas e colunas e o nível de dificuldade.

@param solution  : Grid com a solução final.
@param rowsHints : Lista de listas de inteiros representando as dicas do tabuleiro para as linhas.
@param colsHints : Lista de listas de inteiros representando as dicas do tabuleiro para as colunas.
@param difficulty: Nível de dificuldade do jogo.
@return: Objeto do tipo 'Game'.
-}
data Game = Game {
    solution   :: Grid,      
    rowsHints  :: [[Int]],   
    colsHints  :: [[Int]],   
    difficulty :: Difficulty 
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
Representa o estado dinâmico do jogo durante sua execução.

Contém o grid atual, número de vidas restantes, a estrutura estática do jogo,
status de resolução e a célula atualmente selecionada (cursor).

@param currentGrid  : Grid atual do jogo.
@param lives        : Número de vidas restantes.
@param game         : Estrutura estática do jogo.
@param isSolved     : Indica se o jogo foi resolvido.
@param selectedCell : Tupla com as coordenadas da célula selecionada.
@return: Objeto do tipo 'GameState'.
-}
data GameState = GameState {
    currentGrid  :: Grid,
    lives        :: Int,
    game         :: Game,
    isSolved     :: Bool,
    selectedCell :: (Int, Int) 
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

{-|
Inicializa o estado do jogo a partir de uma estrutura 'Game'.

@param jogo: Objeto do tipo 'Game' com a estrutura estática do jogo.
@return: Estado inicial do jogo (GameState).
-}
initGame :: Game -> GameState
initGame jogo = GameState
    { currentGrid  = replicate linhas (replicate colunas Empty) -- Cria um grid com todas as células marcadas como vazias.
    , lives        = 3
    , game         = jogo
    , isSolved     = False
    , selectedCell = (0, 0)  -- Posiciona o cursor na célula (0,0).
    }
  where
    linhas  = length (solution jogo)       -- Número de linhas da solução.
    colunas = length (head (solution jogo))  -- Número de colunas (baseado na primeira linha).

