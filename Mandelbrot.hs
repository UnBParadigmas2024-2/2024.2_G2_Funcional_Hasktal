-- fiz apenas essa versao de teste baseada em um codigo que achei 
-- se compila usando:  ghc -o mandelbrot Mandelbrot.hs
-- se executa usando: ./mandelbrot

module Main where

import Data.Complex

-- Definicao dos parametros do fractal
width, height :: Int -- da pra brincar com esses valores aqui
width = 80
height = 40

-- vamos passar esses parametros para interacao, da pra brincar com outros valores
maxIter :: Int
maxIter = 100

-- funcaoque determina se um ponto pertence ao fractal
mandelbrot :: Complex Double -> Int
mandelbrot c = length . take maxIter . takeWhile (\z -> magnitude z <= 2) $ iterate (\z -> z^2 + c) 0

-- Converte o o indice de cada interacao em um caractere ASCII
charForIter :: Int -> Char
charForIter n
    | n == maxIter = ' '
    | otherwise    = ['.', ',', '-', '~', ':', ';', '=', '!', '*', '#', '$', '@'] !! (n `mod` 12)

-- converte as coordenadas para um numero complexo
scale :: (Int, Int) -> Complex Double
scale (x, y) = (fromIntegral x / fromIntegral width * 3.5 - 2.5) :+ (fromIntegral y / fromIntegral height * 2 - 1)

-- desenhando o fractal no terminal
drawMandelbrot :: IO ()
drawMandelbrot = mapM_ putStrLn [[charForIter . mandelbrot . scale $ (x, y) | x <- [0..width]] | y <- [0..height]]

-- main
main :: IO ()
main = drawMandelbrot
