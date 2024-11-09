module SierpinskiCarpet (drawSierpinskiCarpet, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))


type MyPoint = (Float, Float)
type Square = (MyPoint, Float)  -- Representa um quadrado como ponto superior esquerdo e tamanho
type GameState = Int

-- Função recursiva que gera o Tapete de Sierpinski
sierpinskiCarpet :: Int -> Square -> [Square]
sierpinskiCarpet 0 sq = [sq]  -- Base: apenas o quadrado inicial
sierpinskiCarpet n (p, size) =
    concatMap (sierpinskiCarpet (n - 1)) smallerSquares
  where
    -- Divide o quadrado em uma grade 3x3 e remove o quadrado do meio
    smallerSquares = [ (movePoint p (dx * newSize, dy * newSize), newSize) 
                     | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0) ]
    newSize = size / 3  -- Tamanho de cada quadrado filho é 1/3 do pai

-- Função auxiliar para calcular a posição de um ponto
movePoint :: MyPoint -> MyPoint -> MyPoint
movePoint (x, y) (dx, dy) = (x + dx, y + dy)

-- Função para desenhar um quadrado, com cores diferentes por profundidade
drawSquare :: Int -> Square -> Picture
drawSquare depth ((x, y), size) =
    let sqColor = if depth == 0
                  then makeColor 0 0 0 1
                  else makeColor (fromIntegral depth / 7) (1 - fromIntegral depth / 7) 0.7 1
    in Color sqColor (translate x y (rectangleSolid size size))

drawSierpinskiCarpet :: GameState -> Picture
drawSierpinskiCarpet gameState = Pictures 
    [ Pictures (map (drawSquare gameState) (sierpinskiCarpet gameState ((0, 0), 400)))  -- Desenha o tapete com o quadrado inicial
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)           -- Exibe o número de iterações
    , Translate (-350) 200 $ button (-1)                                                      -- Botão de decremento
    , Translate 300 200 $ button 1                                                            -- Botão de incremento
    ]

-- Função para desenhar o botão de incremento/decremento
button :: Int -> Picture
button sign = Pictures 
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

-- Manipula os eventos do mouse para aumentar/diminuir o número de iterações
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1
handleEvent _ gameState = gameState

-- Atualiza o estado (neste caso, nada muda com o tempo)
updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState