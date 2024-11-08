module Main (main) where

import Data.Complex (Complex(..), realPart, imagPart)  -- Importando as funções realPart e imagPart
import Graphics.Gloss                                 -- Para a interface gráfica
import Text.Read (readMaybe)                          -- Para ler os argumentos

type R = Float                                       -- O tipo real, pode ser alterado para Double para mais precisão
type C = Complex R                                   -- O tipo complexo
type IterF = C -> C                                  -- Função de iteração do fractal

type Pix        = Color                              -- A cor usada no Gloss
type View       = (C, C)                             -- Parte superior esquerda, parte inferior direita do espaço do fractal
type Dimensions = (Int, Int)                         -- Largura, altura da imagem

inf :: R
inf = 1e20

-- Função base para gerar o fractal (Mandelbrot)
baseFunction :: C -> IterF              
baseFunction c z = z**2 + c

-- Função característica que determina a cor de cada pixel
characteristic  :: (C -> IterF)              -- Função de iteração
                -> R                         -- Raio de escape
                -> Int                       -- Número de iterações
                -> C                         -- Parâmetro complexo
                -> Pix                       -- Cor gerada
characteristic f r n c =
    let g = f c
        l = map snd $
                takeWhile (\(i, z) -> i <= n && realPart (abs z) < r) $
                     zip [1..] $ iterate g (0.0 :+ 0.0)
    in colorFunc n (length l) c (last l)

-- Função para calcular a cor do pixel baseado no número de iterações
colorFunc :: Int                   -- Máximo de iterações
          -> Int                   -- Iterações feitas
          -> C                     -- Número complexo atual
          -> C                     -- Número complexo final após a iteração
          -> Pix                   -- Cor gerada
colorFunc n m z z' =
    let w = 1 + (realPart $ abs z)
        w'= 1 + (realPart $ abs z')
        x = fromIntegral (n - m) / (fromIntegral n)
        r = normal (0.8)  0.05 x
        g = normal (0.8)  0.1 x
        b = sigmoidCut (0.8) 0.05 x

        r'= floor $ r * 255
        g'= floor $ g * 255
        b'= floor $ b * 255
    in makeColorI r' g' b' 255

-- Normaliza o valor para uma distribuição gaussiana
normal :: R -> R -> R -> R
normal c spread x = exp (- (abs (x - c))**2 / spread)

-- Função sigmoidal para cortar a intensidade de cor
sigmoidCut :: R -> R -> R -> R
sigmoidCut c s x = if x < c then 0.0 else -0.5 + 1.0 / (1 + exp (-(x - c)/s))

-- Função que gera a imagem do fractal
produceFractal :: (C -> IterF)         -- Função de iteração
               -> Int                  -- Número de iterações
               -> R                    -- Raio de escape
               -> View                 -- Região do fractal a ser exibida
               -> Dimensions           -- Dimensões da imagem
               -> Picture              -- Imagem gerada para ser exibida
produceFractal f n r (upperLeft, lowerRight) (w, h) =
    let viewWidth = realPart (lowerRight - upperLeft)
        viewHeight= imagPart (upperLeft - lowerRight)

        charProducer = characteristic f r n

        -- Gerando os pixels da imagem
        pixelSpace = [(x, y) | x <- [0..w-1], y <- [0..h-1]]

        -- Função para gerar um pixel com base nas coordenadas
        genFunction :: Int -> Int -> Picture
        genFunction x y =
            let dx =  (fromIntegral x * viewWidth)  / (fromIntegral w)
                dy = -(fromIntegral y * viewHeight) / (fromIntegral h)
                z  = upperLeft + (dx :+ dy)
            in translate (fromIntegral x - fromIntegral (w `div` 2)) 
                        (fromIntegral y - fromIntegral (h `div` 2))
                        (color (charProducer z) (circle 1))

    in pictures $ map (\(x, y) -> genFunction x y) pixelSpace

-- Função principal que processa os argumentos e exibe a janela
main :: IO ()
main = do
    let iterations = 60
        escapeRad = 10.0
        upperLeftX = -1.8
        upperLeftY = 1.3
        lowerRightX = 0.8
        lowerRightY = -1.3
        width = 600
        height = 600

        -- Configurações da região para o fractal
        fractalRegion = (upperLeftX :+ upperLeftY, lowerRightX :+ lowerRightY)

        -- Gerar a imagem
        fractalImage = produceFractal baseFunction iterations escapeRad fractalRegion (width, height)

    -- Exibir a imagem na janela
    display (InWindow "Fractal" (width, height) (100, 100)) white fractalImage
