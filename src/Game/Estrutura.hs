module Game.Estrutura
    ( Cell(..)
    , Grid
    , Difficulty(..)
    , Game(..)
    , GameState(..)
    , initGame
    ) where

-- Uma célula pode estar: [vazia], [preenchida corretamente] ou [marcada incorretamente]
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
-- O campo selectedCell para acompanha a célula atualmente selecionada.
data GameState = GameState {
    currentGrid  :: Grid,       -- Tabuleiro em jogo (por exemplo, inicialmente vazio)
    lives        :: Int,        -- Vidas restantes
    game         :: Game,       -- Estrutura estática do jogo
    isSolved     :: Bool,       -- Indica se o jogo foi resolvido
    selectedCell :: (Int, Int)  -- Coordenadas da célula selecionada (cursor)
} deriving (Eq, Show)

-- Função para iniciar o tabuleiro, recebe um modelo e retorna o 1o GameState
initGame :: Game -> GameState
initGame jogo = GameState
    { currentGrid  = replicate linhas (replicate colunas Empty) -- cria uma matriz vazia
    , lives        = 3
    , game         = jogo
    , isSolved     = False
    , selectedCell = (0, 0)  -- inicia com o cursor na posição (0,0)
    }
  where
    linhas  = length (solution jogo)        -- Qtde de linhas da solução
    colunas = length (head (solution jogo))   -- Tamanho da 1ª linha da solução
