module Renderizacao where

import Graphics.Gloss
import Data.Array

import Data.Maybe
import Jogo

boardGridColor = makeColorI 0 0 0 255
jogadorVerde = makeColorI 0 255 0 255
jogadorMovimento = orange
jogadorVermelho = makeColorI 255 0 0 255
corVencedora = green
validCollor = makeColorI 255 255 255 255
invalidCollor = makeColorI 0 0 0 255
tieColor = greyN 0.5

boardAsRunningPicture board =
   pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color jogadorVerde $ pecasVerdes board
            , color jogadorVermelho $ pecasVermelhas board
            , color boardGridColor $ boardGrid
            ]

boardAsMovingPicture game
   | tipoJogador game == Vermelho = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color jogadorVerde $ pecasVerdes board
            , color jogadorVermelho $ pecasVermelhas board
            , color jogadorMovimento $ selectPecaVermelha board cell
            , color boardGridColor boardGrid
            ]
   | tipoJogador game == Verde = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color jogadorVerde $ pecasVerdes board
            , color jogadorVermelho $ pecasVermelhas board
            , color jogadorMovimento $ selectPecaVerde board cell
            , color boardGridColor boardGrid
            ]
   | otherwise = pictures [ color invalidCollor $ invalidCell board
            , color validCollor $ validCell board
            , color jogadorVerde $ pecasVerdes board
            , color jogadorVermelho $ pecasVermelhas board
            , color boardGridColor boardGrid
            ]
   where board = gameBoard game
         cell = ultimoClick game

outcomeColor (Just Vermelho) = jogadorVermelho
outcomeColor (Just Verde) = jogadorVerde
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
   where x = fromIntegral column * larguraCelula + larguraCelula * 0.5
         y = fromIntegral row * alturaCelula + alturaCelula * 0.5

pecasTabuleiro :: Board -> Celula -> Picture -> Picture
pecasTabuleiro board cell cellPicture =
   pictures
   $ map (snapPictureToCell cellPicture .fst)
   $ filter (\(_, e) -> e == cell)
   $ assocs board

selectPeca :: Board -> (Int,Int) -> Picture -> Picture
selectPeca board (x,y) cell =
   pictures
   $ map (snapPictureToCell cell .fst)
   $ filter (\((a,b), _) -> (a,b) == (x,y))
   $ assocs board
invalid (x,y) = not ((mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0))
valid (x,y) = (mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0)

invalidCell :: Board -> Picture
invalidCell board =
   pictures
   $ map (snapPictureToCell (rectangleSolid larguraCelula alturaCelula) .fst)
   $ filter (\((a,b), _) -> invalid (a,b))
   $ assocs board

validCell :: Board -> Picture
validCell board = 
   pictures
   $ map (snapPictureToCell (rectangleSolid larguraCelula alturaCelula) .fst)
   $ filter (\((a,b), _) -> valid (a,b))
   $ assocs board

selectPecaVerde :: Board -> (Int, Int) -> Picture
selectPecaVerde board point = selectPeca board point cell

selectPecaVermelha :: Board -> (Int, Int) -> Picture
selectPecaVermelha board point = selectPeca board point cell

cell :: Picture
cell = thickCircle radius 15.0
   where radius = min larguraCelula alturaCelula * 0.25

pecasVermelhas :: Board -> Picture
pecasVermelhas board = pecasTabuleiro board (Full Vermelho) cell

pecasVerdes :: Board -> Picture
pecasVerdes board = pecasTabuleiro board (Full Verde) cell


boardGrid :: Picture
boardGrid =
   pictures
   $ concatMap (\i -> [ line [ (i * larguraCelula, 0.0),
                               (i * larguraCelula, fromIntegral alturaTela)
                             ]
                      , line [ (0.0, i * alturaCelula)
                             , (fromIntegral larguraTela, i * alturaCelula)
                             ]
                        ])
   [0.0 .. fromIntegral n]


boardAsPicture board =
   pictures [ pecasVermelhas board
   , pecasVerdes board
   , boardGrid
   ]

boardAsGameOverPicture board winner
   | isNothing winner = pictures [ color tieColor $ pecasVerdes board
            , color tieColor $ pecasVermelhas board
            , color tieColor boardGrid
            ]
   | otherwise = pictures [ color corVencedora $ pecasVerdes board
            , color corVencedora $ pecasVermelhas board
            , color corVencedora boardGrid
            ]

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral larguraTela * (-0.5)) (fromIntegral alturaTela * (-0.5)) frame
   where frame = case statusJogo game of
                    Move -> boardAsMovingPicture game
                    GameOver winner -> boardAsGameOverPicture (gameBoard game) winner
                    _ -> boardAsRunningPicture (gameBoard game)
