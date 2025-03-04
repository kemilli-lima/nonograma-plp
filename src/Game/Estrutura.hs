{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}

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

-- Uma célula pode estar: [vazia], [preenchida corretamente] ou [marcada incorretamente]
data Cell = Empty | Filled | Marked deriving (Eq, Generic, ToJSON, FromJSON)


instance Show Cell where
    show Empty  = ". "  -- Célula sem decisão
    show Filled = "x "  -- Célula marcada como errada
    show Marked = "* "  -- Célula correta

type Grid = [[Cell]]

data Difficulty = Easy | Medium | Hard deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Estrutura estática do jogo: contém o mapa final (solução), dicas fixas e dificuldade.
data Game = Game {
    solution  :: Grid,      -- Mapa final (solução)
    rowsHints :: [[Int]],   -- Dicas das linhas
    colsHints :: [[Int]],   -- Dicas das colunas
    difficulty :: Difficulty -- Dificuldade (característica fixa do jogo)
} deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Estado dinâmico do jogo: contém o progresso atual, vidas, etc.
data GameState = GameState {
    currentGrid :: Grid,   -- Tabuleiro em jogo (por exemplo, inicialmente vazio)
    lives       :: Int,    -- Vidas restantes
    game        :: Game,   -- Estrutura estática do jogo
    isSolved    :: Bool    -- Indica se o jogo foi resolvido
} deriving (Eq, Show, Generic, ToJSON, FromJSON)


-- Funcao para iniciar o tabuleiro, recebe um modelo e retorna o 1o gamestate
initGame :: Game -> GameState
initGame jogo = GameState
    { currentGrid = replicate linhas (replicate colunas Empty) -- cria uma matriz vazia
    , lives       = 3
    , game        = jogo
    , isSolved    = False
    }
  where
    linhas  = length (solution jogo) -- Pega a qtdd de linhas da matriz
    colunas = length (head (solution jogo)) -- entra no vector da 1a linha e pega o tamanho do vector
