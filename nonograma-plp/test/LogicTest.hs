module LogicTest where

import Test.HUnit
import Game.Estrutura
import Game.Logic

-- Testes para a função updateCellWithCheck
testUpdateCellWithCheckCorrect :: Test
testUpdateCellWithCheckCorrect = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- updateCellWithCheck initialState (0, 0) Filled
    assertEqual "Célula corretamente preenchida" Filled ((currentGrid newState !! 0) !! 0)
    assertEqual "Vidas não devem mudar" 3 (lives newState)

testUpdateCellWithCheckIncorrect :: Test
testUpdateCellWithCheckIncorrect = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- updateCellWithCheck initialState (0, 0) Marked
    assertEqual "Célula incorretamente preenchida" Empty ((currentGrid newState !! 0) !! 0)
    assertEqual "Vidas devem diminuir" 2 (lives newState)

-- Testes para a função checkVictory
testCheckVictoryTrue :: Test
testCheckVictoryTrue = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Filled, Marked], [Marked, Filled]] 3 game False
    assertBool "Jogo deve estar resolvido" (checkVictory state)

testCheckVictoryFalse :: Test
testCheckVictoryFalse = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Empty, Marked], [Marked, Filled]] 3 game False
    assertBool "Jogo não deve estar resolvido" (not (checkVictory state))

-- Testes para a função giveHint
testGiveHint :: Test
testGiveHint = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- giveHint initialState
    assertEqual "Dica deve preencher uma célula corretamente" Filled ((currentGrid newState !! 0) !! 0)

-- Testes para a função isGameOver
testIsGameOverTrue :: Test
testIsGameOverTrue = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Empty, Empty], [Empty, Empty]] 0 game False
    assertBool "Jogo deve estar encerrado" (isGameOver state)

testIsGameOverFalse :: Test
testIsGameOverFalse = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Empty, Empty], [Empty, Empty]] 1 game False
    assertBool "Jogo não deve estar encerrado" (not (isGameOver state))

testUpdateCellWithCheckAlreadyFilled :: Test
testUpdateCellWithCheckAlreadyFilled = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Filled, Empty], [Empty, Empty]] 3 game False
    newState <- updateCellWithCheck initialState (0, 0) Filled
    assertEqual "Célula já preenchida não deve mudar" Filled ((currentGrid newState !! 0) !! 0)
    assertEqual "Vidas não devem mudar" 3 (lives newState)

testUpdateCellWithCheckOutOfBounds :: Test
testUpdateCellWithCheckOutOfBounds = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- updateCellWithCheck initialState (2, 2) Filled
    assertEqual "Célula fora dos limites não deve mudar" Empty ((currentGrid newState !! 0) !! 0)
    assertEqual "Vidas não devem mudar" 3 (lives newState)

testUpdateCellWithCheckCorrectMark :: Test
testUpdateCellWithCheckCorrectMark = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- updateCellWithCheck initialState (0, 1) Marked
    assertEqual "Célula corretamente marcada" Marked ((currentGrid newState !! 0) !! 1)
    assertEqual "Vidas não devem mudar" 3 (lives newState)

testCheckVictoryPartiallyFilled :: Test
testCheckVictoryPartiallyFilled = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Filled, Empty], [Marked, Filled]] 3 game False
    assertBool "Jogo não deve estar resolvido" (not (checkVictory state))

testCheckVictoryIncorrectlyFilled :: Test
testCheckVictoryIncorrectlyFilled = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Filled, Filled], [Filled, Filled]] 3 game False
    assertBool "Jogo não deve estar resolvido" (not (checkVictory state))

testGiveHintOnCompleteGrid :: Test
testGiveHintOnCompleteGrid = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Filled, Marked], [Marked, Filled]] 3 game False
    newState <- giveHint initialState
    assertEqual "Dica não deve mudar o grid" [[Filled, Marked], [Marked, Filled]] (currentGrid newState)

testGiveHintOnEmptyGrid :: Test
testGiveHintOnEmptyGrid = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    newState <- giveHint initialState
    assertEqual "Dica deve preencher uma célula corretamente" Filled ((currentGrid newState !! 0) !! 0)


testIsGameOverLost :: Test
testIsGameOverLost = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let state = GameState [[Empty, Empty], [Empty, Empty]] 0 game False
    assertBool "Jogo deve estar encerrado (perdido)" (isGameOver state)

testIntegration :: Test
testIntegration = TestCase $ do
    let game = Game [[Filled, Marked], [Marked, Filled]] [[1,1],[1,1]] [[1,1],[1,1]] Easy
    let initialState = GameState [[Empty, Empty], [Empty, Empty]] 3 game False
    state1 <- updateCellWithCheck initialState (0, 0) Filled
    state2 <- updateCellWithCheck state1 (0, 1) Marked
    state3 <- giveHint state2
    assertEqual "Célula (0,0) deve estar preenchida" Filled ((currentGrid state3 !! 0) !! 0)
    assertEqual "Célula (0,1) deve estar marcada" Marked ((currentGrid state3 !! 0) !! 1)
    assertEqual "Vidas devem ser 3" 3 (lives state3)




-- Lista de todos os testes
tests :: Test
tests = TestList
    [ TestLabel "testUpdateCellWithCheckCorrect" testUpdateCellWithCheckCorrect
    , TestLabel "testUpdateCellWithCheckIncorrect" testUpdateCellWithCheckIncorrect
    , TestLabel "testCheckVictoryTrue" testCheckVictoryTrue
    , TestLabel "testCheckVictoryFalse" testCheckVictoryFalse
    , TestLabel "testGiveHint" testGiveHint
    , TestLabel "testIsGameOverTrue" testIsGameOverTrue
    , TestLabel "testIsGameOverFalse" testIsGameOverFalse
    , TestLabel "testUpdateCellWithCheckAlreadyFilled" testUpdateCellWithCheckAlreadyFilled
    , TestLabel "testUpdateCellWithCheckOutOfBounds" testUpdateCellWithCheckOutOfBounds
    , TestLabel "testUpdateCellWithCheckCorrectMark" testUpdateCellWithCheckCorrectMark
    , TestLabel "testCheckVictoryPartiallyFilled" testCheckVictoryPartiallyFilled
    , TestLabel "testCheckVictoryIncorrectlyFilled" testCheckVictoryIncorrectlyFilled
    , TestLabel "testGiveHintOnCompleteGrid" testGiveHintOnCompleteGrid
    , TestLabel "testGiveHintOnEmptyGrid" testGiveHintOnEmptyGrid
    , TestLabel "testIsGameOverLost" testIsGameOverLost
    , TestLabel "testIntegration" testIntegration
    ]

-- Função principal para rodar os testes
main :: IO ()
main = do
    runTestTT tests
    return ()