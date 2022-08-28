module Jogo where

import Data.Array

data Jogador = Vermelho | Verde deriving (Eq, Show)
data Celula = Empty | Full Jogador deriving (Eq, Show)
data State = Move | Running | GameOver (Maybe Jogador) deriving (Eq, Show)

type Board = Array (Int, Int) Celula
data Game = Game { gameBoard :: Board
                 , tipoJogador :: Jogador
                 , statusJogo :: State
                 , ultimoClick :: (Int, Int)
                 , changeRock :: Bool
                 } deriving (Eq, Show)
n :: Int
n = 8

larguraTela :: Int
larguraTela = 640

alturaTela :: Int
alturaTela = 640

alturaCelula::Float
alturaCelula = fromIntegral alturaTela / fromIntegral n

larguraCelula :: Float
larguraCelula = fromIntegral larguraTela / fromIntegral n

inicioJogo = Game { gameBoard = inicioJogoTab
                   , tipoJogador = Vermelho
                   , statusJogo = Running
                   , ultimoClick = (-1,-1)
                   , changeRock = True
                  }

inicioJogoTab :: Board
inicioJogoTab = (array indexRange peçasIniciais)
{--[((0,0), Full Vermelho), ((1,1), Full Vermelho), ((7,7), Full Verde), ((6,6), Full Verde), ((7,5), Full Verde), ((7,3), Full Verde), ((7,1), Full Verde), ((6,4), Full Verde), ((6,2), Full Verde), ((6,0), Full Verde)] --}
   where indexRange = ((0,0), (n-1, n-1))

peçasIniciais :: [((Int, Int),Celula)]
peçasIniciais = [((x, y), jogador (x, y)) | x<-[0..7], y<-[0..7]]
{--   zip (range indexRange) [((0,0), Full Vermelho), ((1,1), Full Vermelho), ((7,7), Full Verde), ((6,6), Full Verde), ((7,5), Full Verde), ((7,3), Full Verde), ((7,1), Full Verde), ((6,4), Full Verde), ((6,2), Full Verde), ((6,0), Full Verde)]
 --}  where indexRange = ((0,0), (n-1, n-1))

jogador :: (Int, Int) -> Celula
jogador (x,y)
   | piece && x <= 2 = Full Vermelho
   | piece && x >= 5 = Full Verde
   | otherwise = Empty
   where
      piece = (mod y 2 == 1 && mod x 2 == 1) || (mod y 2 == 0 && mod x 2 == 0)
