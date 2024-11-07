module Main (main) where

import Graphics.Gloss
import Lib (sierpinski, drawTriangle)

main :: IO ()
main = display (InWindow "Triângulo de Sierpinski" (800, 800) (450, 450)) white 
        (Pictures (map drawTriangle (sierpinski 10 ((-350, -350), (350, -350), (0, 350)))))

