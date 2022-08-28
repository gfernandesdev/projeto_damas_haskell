module Main where

import Data.Array
import Jogo
import Logica
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain $ tests [1,2,3]
    print ""

tests :: [Double] -> TestTree
tests dataset = testGroup "Testes da Atividade 6" 
        [ jogadorTest
        , peçasIniciaisTest
        , inicioJogoTabTest
        , inicioJogoTest
        , coordCorretaTest
        , jogadorInversoTest
        , proxJogadorTest
        , possivelMovVermelhoTest
        , possivelMovVerdeTest
        , playerTurnTest
        , movePecaVerdeTest
        , movePecaVermelhoTest
        , statusJogoAposMovTest]

statusJogoAposMovTest = testGroup "statusJogoAposMov" 
            [ testCase "Test 1 - statusJogoAposMov otherwise" (assertEqual "Test 1" Running (statusJogoAposMov inicioJogo))
            , testCase "Test 2 - statusJogoAposMov GameOver Just Vermelho" (assertEqual "Test 1" (GameOver (Just Vermelho)) (statusJogoAposMov tableJustVermelho))
            , testCase "Test 3 - statusJogoAposMov GameOver Just Verde" (assertEqual "Test 1" (GameOver (Just Verde)) (statusJogoAposMov tableJustVerde))
            , testCase "Test 4 - statusJogoAposMov GameOver Nothing" (assertEqual "Test 1" (GameOver Nothing) (statusJogoAposMov tableOneVermelhoOneVerde))
            ]

movePecaVerdeTest = testGroup "movePecaVerde" 
            [ testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVerde inicioJogo (9,9)))
            , testCase "Test 2 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVerde inicioJogo (9,9)))
            , testCase "Test 3 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVerde inicioJogo (9,9)))
            , testCase "Test 4 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVerde inicioJogo (9,9)))
            , testCase "Test 5 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVerde inicioJogo (9,9)))
            ]

movePecaVermelhoTest = testGroup "movePecaVermelho" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (movePecaVermelho inicioJogo (9,9)))
            ]

playerTurnTest = testGroup "playerTurn" 
            [ testCase "Test 1 - Click em 9,9" (assertEqual "Test 1" playerTurnInvalid (playerTurn inicioJogoMock (9,9)))
            , testCase "Test 1 - Click em 3,3" (assertEqual "Test 1" playerTurnInvalid (playerTurn inicioJogoMock (3,3)))
            , testCase "Test 1 - Click em 2,2" (assertEqual "Test 1" playerTurn22 (playerTurn inicioJogoMock (2,2)))
            , testCase "Test 1 - Click em 5,7" (assertEqual "Test 1" playerTurnInvalid (playerTurn inicioJogoMock (5,7)))
            ]

possivelMovVerdeTest = testGroup "possivelMovVerde" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (possivelMovVerde inicioJogo (9,9)))
            ]

possivelMovVermelhoTest = testGroup "possivelMovVermelho" 
            [ testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" inicioJogo (possivelMovVermelho inicioJogo (9,9)))
            , testCase "Test 2 - Click na pedra em 2,2" (assertEqual "Test 1" afterPossibleMoves22 (possivelMovVermelho inicioJogo (2,2)))
            ]

proxJogadorTest = testGroup "proxJogador" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" proxJogadorInitialResponse (proxJogador inicioJogo (0,0) (1,1) (0,0)))
            ]

inicioJogoTest = testGroup "inicioJogo" 
            [testCase "Test 1 - tabuleiro inicial" (assertEqual "Test 1" inicioJogoMock inicioJogo)
            ]

inicioJogoTabTest = testGroup "inicioJogoTab" 
            [testCase "Test 1 - tabuleiro" (assertEqual "Test 1" tabuleiro inicioJogoTab)
            ]

peçasIniciaisTest = testGroup "peçasIniciais" 
            [testCase "Test 1 - peças inicias" (assertEqual "Test 1" peças peçasIniciais)
            ]

jogadorTest = testGroup "jogadorTest" 
            [testCase "Test 1 - Jogador na posição inicial correta" (assertEqual "Test 1" (Full Vermelho) (jogador (0,0))),
            testCase "Test 2 - Jogador na posição inicial correta" (assertEqual "Test 2" (Full Vermelho) (jogador (2,0))),
            testCase "Test 3 - Jogador na posição inicial correta" (assertEqual "Test 3" (Full Vermelho) (jogador (1,1))),
            testCase "Test 4 - Jogador na posição inicial correta" (assertEqual "Test 4" (Full Vermelho) (jogador (2,2))),
            testCase "Test 5 - Jogador na posição inicial correta" (assertEqual "Test 5" (Full Vermelho) (jogador (1,7))),
            testCase "Test 6 - Jogador na posição inicial correta" (assertEqual "Test 6" (Full Verde) (jogador (7,7))),
            testCase "Test 7 - Jogador na posição inicial correta" (assertEqual "Test 7" (Full Verde) (jogador (6,6))),
            testCase "Test 8 - Jogador na posição inicial correta" (assertEqual "Test 8" (Full Verde) (jogador (5,5))),
            testCase "Test 9 - Jogador na posição inicial correta" (assertEqual "Test 9" (Full Verde) (jogador (7,3))),
            testCase "Test 10 - Jogador na posição inicial correta" (assertEqual "Test 10" (Full Verde) (jogador (7,1))),
            testCase "Test 11 - Jogador na posição inicial correta" (assertEqual "Test 11" Empty (jogador (4,4))),
            testCase "Test 12 - Jogador na posição inicial correta" (assertEqual "Test 12" Empty (jogador (3,3))),
            testCase "Test 13 - Jogador na posição inicial correta" (assertEqual "Test 13" Empty (jogador (4,0))),
            testCase "Test 14 - Jogador na posição inicial correta" (assertEqual "Test 14" Empty (jogador (0,1))),
            testCase "Test 15 - Jogador na posição inicial correta" (assertEqual "Test 15" Empty (jogador (7,6)))
            ]

jogadorInversoTest = testGroup "jogadorInversoTest" 
            [testCase "Test 1 - Vermelho to Verde" (assertEqual "Test 1" Verde (jogadorInverso Vermelho)),
            testCase "Test 2 - Verde to Vermelho" (assertEqual "Test 2" Vermelho (jogadorInverso Verde))
            ]

coordCorretaTest = testGroup "coordCorretaTest" 
            [testCase "Test 1 - (0,0)" (assertEqual "Test 1" True (coordCorreta (0,0))),
            testCase "Test 2 - (7,7)" (assertEqual "Test 2" True (coordCorreta (7,7))),
            testCase "Test 3 - (8,8)" (assertEqual "Test 3" False (coordCorreta (8,8))),
            testCase "Test 4 - (-1,-1)" (assertEqual "Test 4" False (coordCorreta (-1,-1)))
            ]

inicioJogoMock = Game {gameBoard = tabuleiro, tipoJogador = Vermelho, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}

tabuleiro = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Full Vermelho),((0,3),Empty),((0,4),Full Vermelho),((0,5),Empty),((0,6),Full Vermelho),((0,7),Empty),((1,0),Empty),((1,1),Full Vermelho),((1,2),Empty),((1,3),Full Vermelho),((1,4),Empty),((1,5),Full Vermelho),((1,6),Empty),((1,7),Full Vermelho),((2,0),Full Vermelho),((2,1),Empty),((2,2),Full Vermelho),((2,3),Empty),((2,4),Full Vermelho),((2,5),Empty),((2,6),Full Vermelho),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Verde),((5,2),Empty),((5,3),Full Verde),((5,4),Empty),((5,5),Full Verde),((5,6),Empty),((5,7),Full Verde),((6,0),Full Verde),((6,1),Empty),((6,2),Full Verde),((6,3),Empty),((6,4),Full Verde),((6,5),Empty),((6,6),Full Verde),((6,7),Empty),((7,0),Empty),((7,1),Full Verde),((7,2),Empty),((7,3),Full Verde),((7,4),Empty),((7,5),Full Verde),((7,6),Empty),((7,7),Full Verde)]

peças = [((x, y), jogador (x, y)) | x<-[0..7], y<-[0..7]]

proxJogadorInitialResponse = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Full Vermelho),((0,3),Empty),((0,4),Full Vermelho),((0,5),Empty),((0,6),Full Vermelho),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Full Vermelho),((1,4),Empty),((1,5),Full Vermelho),((1,6),Empty),((1,7),Full Vermelho),((2,0),Full Vermelho),((2,1),Empty),((2,2),Full Vermelho),((2,3),Empty),((2,4),Full Vermelho),((2,5),Empty),((2,6),Full Vermelho),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Verde),((5,2),Empty),((5,3),Full Verde),((5,4),Empty),((5,5),Full Verde),((5,6),Empty),((5,7),Full Verde),((6,0),Full Verde),((6,1),Empty),((6,2),Full Verde),((6,3),Empty),((6,4),Full Verde),((6,5),Empty),((6,6),Full Verde),((6,7),Empty),((7,0),Empty),((7,1),Full Verde),((7,2),Empty),((7,3),Full Verde),((7,4),Empty),((7,5),Full Verde),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Verde, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}

tableJustVermelho = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Empty)], tipoJogador = Verde, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}

tableJustVerde = Game {gameBoard = array ((0,0),(7,7)) [((0,0), Empty),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Verde, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}

tableOneVermelhoOneVerde = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Empty),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Empty),((2,3),Empty),((2,4),Empty),((2,5),Empty),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Empty),((5,5),Empty),((5,6),Empty),((5,7),Empty),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Empty),((6,5),Empty),((6,6),Empty),((6,7),Empty),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Verde, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}

afterPossibleMoves22 = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Full Vermelho),((0,3),Empty),((0,4),Full Vermelho),((0,5),Empty),((0,6),Full Vermelho),((0,7),Empty),((1,0),Empty),((1,1),Full Vermelho),((1,2),Empty),((1,3),Full Vermelho),((1,4),Empty),((1,5),Full Vermelho),((1,6),Empty),((1,7),Full Vermelho),((2,0),Full Vermelho),((2,1),Empty),((2,2),Full Vermelho),((2,3),Empty),((2,4),Full Vermelho),((2,5),Empty),((2,6),Full Vermelho),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Verde),((5,2),Empty),((5,3),Full Verde),((5,4),Empty),((5,5),Full Verde),((5,6),Empty),((5,7),Full Verde),((6,0),Full Verde),((6,1),Empty),((6,2),Full Verde),((6,3),Empty),((6,4),Full Verde),((6,5),Empty),((6,6),Full Verde),((6,7),Empty),((7,0),Empty),((7,1),Full Verde),((7,2),Empty),((7,3),Full Verde),((7,4),Empty),((7,5),Full Verde),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Vermelho, statusJogo = Move, ultimoClick = (2,2), changeRock = True}

playerTurn22 = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Full Vermelho),((0,3),Empty),((0,4),Full Vermelho),((0,5),Empty),((0,6),Full Vermelho),((0,7),Empty),((1,0),Empty),((1,1),Full Vermelho),((1,2),Empty),((1,3),Full Vermelho),((1,4),Empty),((1,5),Full Vermelho),((1,6),Empty),((1,7),Full Vermelho),((2,0),Full Vermelho),((2,1),Empty),((2,2),Full Vermelho),((2,3),Empty),((2,4),Full Vermelho),((2,5),Empty),((2,6),Full Vermelho),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Verde),((5,2),Empty),((5,3),Full Verde),((5,4),Empty),((5,5),Full Verde),((5,6),Empty),((5,7),Full Verde),((6,0),Full Verde),((6,1),Empty),((6,2),Full Verde),((6,3),Empty),((6,4),Full Verde),((6,5),Empty),((6,6),Full Verde),((6,7),Empty),((7,0),Empty),((7,1),Full Verde),((7,2),Empty),((7,3),Full Verde),((7,4),Empty),((7,5),Full Verde),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Vermelho, statusJogo = Move, ultimoClick = (2,2), changeRock = True}
playerTurnInvalid = Game {gameBoard = array ((0,0),(7,7)) [((0,0),Full Vermelho),((0,1),Empty),((0,2),Full Vermelho),((0,3),Empty),((0,4),Full Vermelho),((0,5),Empty),((0,6),Full Vermelho),((0,7),Empty),((1,0),Empty),((1,1),Full Vermelho),((1,2),Empty),((1,3),Full Vermelho),((1,4),Empty),((1,5),Full Vermelho),((1,6),Empty),((1,7),Full Vermelho),((2,0),Full Vermelho),((2,1),Empty),((2,2),Full Vermelho),((2,3),Empty),((2,4),Full Vermelho),((2,5),Empty),((2,6),Full Vermelho),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Empty),((3,5),Empty),((3,6),Empty),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Empty),((4,4),Empty),((4,5),Empty),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Full Verde),((5,2),Empty),((5,3),Full Verde),((5,4),Empty),((5,5),Full Verde),((5,6),Empty),((5,7),Full Verde),((6,0),Full Verde),((6,1),Empty),((6,2),Full Verde),((6,3),Empty),((6,4),Full Verde),((6,5),Empty),((6,6),Full Verde),((6,7),Empty),((7,0),Empty),((7,1),Full Verde),((7,2),Empty),((7,3),Full Verde),((7,4),Empty),((7,5),Full Verde),((7,6),Empty),((7,7),Full Verde)], tipoJogador = Vermelho, statusJogo = Running, ultimoClick = (-1,-1), changeRock = True}
