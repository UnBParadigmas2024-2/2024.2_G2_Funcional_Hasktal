module Main (main) where

import Graphics.Gloss
import Sierpinski (drawSierpinski, handleEvent, updateState)

main :: IO ()
main = play (InWindow "Triangulo de Sierpinski" (800, 800) (450, 450)) 
            white 60 0 drawSierpinski handleEvent updateState


