module Main where

import Graphics.Gloss
import Mandelbrot (drawMandelbrot, handleEvent, updateState)

main :: IO ()
main = play (InWindow "Fractal de Mandelbrot" (800, 800) (450, 450)) 
            white 60 0 drawMandelbrot handleEvent updateState