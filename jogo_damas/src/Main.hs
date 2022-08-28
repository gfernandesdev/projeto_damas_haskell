module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Jogo
import Logica
import Renderizacao

window = InWindow "Damas" (640, 640) (100, 100)
backgroundColor = blue

main :: IO ()
main = play window backgroundColor 30  inicioJogo gameAsPicture transfJogo (\_->id)
