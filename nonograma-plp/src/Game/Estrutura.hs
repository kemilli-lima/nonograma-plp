module Game.Estrutura
    ( Cell(..)
    , Grid
    , Difficulty(..)
    , Game(..)
    , GameState(..)
    , initGame
    ) where

-- Uma célula pode estar: vazia, preenchida incorretamente ou marcada corretamente
data Cell = Empty | Filled | Marked deriving (Eq)

instance Show Cell where
    show Empty  = ". "  -- Célula sem decisão
    show Filled = "x "  -- Célula marcada como errada
    show Marked = "* "  -- Célula correta

type Grid = [[Cell]]

data Difficulty = Easy | Medium | Hard deriving (Eq, Show)

-- Estrutura estática do jogo: contém o mapa final (solução), dicas fixas e dificuldade.
data Game = Game {
    solution  :: Grid,      -- Mapa final (solução)
    rowsHints :: [[Int]],   -- Dicas das linhas
    colsHints :: [[Int]],   -- Dicas das colunas
    difficulty :: Difficulty -- Dificuldade (característica fixa do jogo)
} deriving (Eq, Show)

-- Estado dinâmico do jogo: contém o progresso atual, vidas, etc.
data GameState = GameState {
    currentGrid :: Grid,   -- Tabuleiro em jogo (por exemplo, inicialmente vazio)
    lives       :: Int,    -- Vidas restantes
    game        :: Game,   -- Estrutura estática do jogo
    isSolved    :: Bool    -- Indica se o jogo foi resolvido
} deriving (Eq, Show)


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
