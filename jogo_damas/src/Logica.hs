module Logica where
import Data.Array
import Jogo
import Graphics.Gloss.Interface.Pure.Game

coordCorreta = inRange ((0,0), (n-1, n-1))

jogadorInverso :: Jogador -> Jogador
jogadorInverso player
   | player == Verde = Vermelho
   | otherwise = Verde

proxJogador :: Game -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Game
proxJogador game (x,y) (a,b) ultCelula
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x+2, y+2) && board ! (x+2, y+2) == Empty = game {gameBoard = board // [(ultCelula, Empty), ((a, b), Empty), ((x,y), Full $ player)],  tipoJogador = player, statusJogo = Move, ultimoClick = (x,y), changeRock = False}
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x-2, y+2) && board ! (x-2, y+2) == Empty = game {gameBoard = board // [(ultCelula, Empty), ((a, b), Empty), ((x,y), Full $ player)],  tipoJogador = player, statusJogo = Move, ultimoClick = (x,y), changeRock = False}
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x-2, y-2) && board ! (x-2, y-2) == Empty = game {gameBoard = board // [(ultCelula, Empty), ((a, b), Empty), ((x,y), Full $ player)],  tipoJogador = player, statusJogo = Move, ultimoClick = (x,y), changeRock = False}
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x+2, y-2) && board ! (x+2, y-2) == Empty = game {gameBoard = board // [(ultCelula, Empty), ((a, b), Empty), ((x,y), Full $ player)],  tipoJogador = player, statusJogo = Move, ultimoClick = (x,y), changeRock = False}
   | otherwise = game {gameBoard = board // [(ultCelula, Empty), ((a,b), Empty), ((x, y), Full $ player)],  tipoJogador = jogadorInverso player, statusJogo = Running, changeRock = True}
   where board = gameBoard game
         player = tipoJogador game


possivelMov :: Game -> (Int, Int) -> Game
possivelMov game coord
      | tipoJogador game == Vermelho = possivelMovVermelho game coord
      | otherwise = possivelMovVerde game coord

possivelMovVermelho :: Game -> (Int, Int) -> Game
possivelMovVermelho game (x,y)
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y+2) && board ! (x+2,y+2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y-2) && board ! (x+2,y-2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y-2) && board ! (x-2,y-2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y+2) && board ! (x-2,y+2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | otherwise = game
   where board = gameBoard game
         player = tipoJogador game
         ultCelula = ultimoClick game

possivelMovVerde :: Game -> (Int, Int) -> Game
possivelMovVerde game (x,y)
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y+2) && board ! (x+2,y+2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y-2) && board ! (x+2,y-2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y-2) && board ! (x-2,y-2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y+2) && board ! (x-2,y+2) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == Empty = game {gameBoard = board, statusJogo = Move, ultimoClick = (x,y)}
   | otherwise = game
   where board = gameBoard game
         player = tipoJogador game
         ultCelula = ultimoClick game

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
   | statusJogoAposMov game == GameOver (Just Vermelho) = game {statusJogo = GameOver (Just Vermelho)}
   | statusJogoAposMov game == GameOver (Just Verde) = game {statusJogo = GameOver (Just Verde)}
   | statusJogoAposMov game == GameOver Nothing = game {statusJogo = GameOver Nothing}
   | coordCorreta cellCoord && board ! cellCoord == (Full $ player) = possivelMov game cellCoord
   | otherwise = game
   where board = gameBoard game
         player = tipoJogador game

movePeca :: Game -> (Int, Int) -> Game
movePeca game cellCoord = if coordCorreta cellCoord then if player == Vermelho then movePecaVermelho game cellCoord else movePecaVerde game cellCoord else game
   where player = tipoJogador game

movePecaVerde :: Game -> (Int, Int) -> Game
movePecaVerde game (x,y)
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y+2) && (x+2,y+2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x+1, y+1) ultCelula
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y-2) && (x+2,y-2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x+1, y-1) ultCelula
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y-2) && (x-2,y-2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x-1,y-1) ultCelula
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y+2) && (x-2,y+2) == ultCelula && board ! (x, y) == Empty = proxJogador game (x,y) (x-1,y+1) ultCelula
   | ((x+1,y-1)==ultCelula || (x+1, y+1) == ultCelula) && board ! (x, y) == Empty  = game {gameBoard = board // [((x, y), Full $ player), (ultCelula, Empty)], tipoJogador = jogadorInverso $ tipoJogador game, statusJogo = statusJogoAposMov game}
   | not (changeRock game) = game
   | otherwise = game {statusJogo = statusJogoAposMov game}
   where board = gameBoard game
         player = tipoJogador game
         ultCelula = ultimoClick game

movePecaVermelho :: Game -> (Int, Int) -> Game
movePecaVermelho game (x,y)
   | coordCorreta (x+1, y+1) && board ! (x+1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y+2) && (x+2,y+2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x+1, y+1) ultCelula
   | coordCorreta (x+1, y-1) && board ! (x+1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x+2,y-2) && (x+2,y-2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x+1, y-1) ultCelula
   | coordCorreta (x-1, y-1) && board ! (x-1, y-1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y-2) && (x-2,y-2) == ultCelula && board ! (x, y) == Empty  = proxJogador game (x,y) (x-1,y-1) ultCelula
   | coordCorreta (x-1, y+1) && board ! (x-1, y+1) == (Full $ jogadorInverso player) && coordCorreta (x-2,y+2) && (x-2,y+2) == ultCelula && board ! (x, y) == Empty = proxJogador game (x,y) (x-1,y+1) ultCelula
   | ((x-1,y-1)==ultCelula || (x-1, y+1) == ultCelula) && board ! (x, y) == Empty  = game {gameBoard = board // [((x, y), Full $ player), (ultCelula, Empty)], tipoJogador = jogadorInverso $ tipoJogador game, statusJogo = statusJogoAposMov game}
   | not (changeRock game) = game
   | otherwise = game {statusJogo = statusJogoAposMov game}
   where board = gameBoard game
         player = tipoJogador game
         ultCelula = ultimoClick game

statusJogoAposMov :: Game -> State
statusJogoAposMov game
   | pecasVerdes == 0 = GameOver (Just Vermelho)
   | pecasVermelhas == 0 = GameOver (Just Verde)
   | allPieces == 2 = GameOver Nothing
   | otherwise = Running
   where
      countPieces :: Celula -> [Celula] -> Int
      countPieces player xs = length $ filter (== player) xs
      pecasVerdes :: Int
      pecasVerdes = countPieces (Full Verde) (elems $ gameBoard game)
      pecasVermelhas :: Int
      pecasVermelhas = countPieces (Full Vermelho) (elems $ gameBoard game)
      allPieces :: Int
      allPieces = pecasVerdes + pecasVermelhas

posCoordCelula :: (Float, Float) -> (Int, Int)
posCoordCelula (x,y) = ( floor ((y + (fromIntegral alturaTela * 0.5))/alturaCelula)
                            , floor ((x + (fromIntegral larguraTela * 0.5))/larguraCelula )
                            )

transfJogo :: Event -> Game -> Game
transfJogo (EventKey (MouseButton LeftButton) Up _ mousePos) game =
   case statusJogo game of
      Running -> playerTurn game $ posCoordCelula mousePos
      Move -> movePeca game $ posCoordCelula mousePos
      GameOver _ -> inicioJogo
transfJogo _ game = game
