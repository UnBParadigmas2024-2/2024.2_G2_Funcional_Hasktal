module FractalTree (drawFractalTree, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))

type Line    = (Point, Point)
type Path    = [Point]
type Picture = [(Colour, FractalTree.Path)]
type Colour  = (Int, Int, Int, Int) -- red, green, blue, opacity

type GameState = Int

-- Função para mover a origem da linha para um ponto específico
startLineFrom :: Point -> Line -> Line
startLineFrom (x0, y0) ((xS, yS), (xE, yE))
  = ((x0, y0), (x0 + xE - xS, y0 + yE - yS)) 

-- Função para mover a segunda linha, tal que começa no ponto final da primeira linha
connectLine :: Line -> Line -> Line
connectLine l1 l2 = startLineFrom (snd l1) l2

-- Função para escalar o comprimento da linha com o fator fornecido
scaleLine :: Float -> Line -> Line
scaleLine f ((x1, y1), (x2, y2))
  = ((x1, y1), (x1 + (x2 - x1) * f , y1 + (y2 - y1) * f))

-- Função para rotacionar uma linha pelo ângulo fornecido
rotateLine :: Float -> Line -> Line
rotateLine alpha l@(p1@(x1, y1), p2@(x2, y2)) 
  = ((x1, y1), ((cos alpha) * nx - (sin alpha) * ny + x1,
                (sin alpha) * nx + (cos alpha) * ny + y1))
  where
    (nx, ny) = (x2 - x1, y2 - y1)

-- Função para dar um "efeito de desvanecimento" na cor
fade :: Colour -> Colour
fade (r, y, b, o)
  | o == 0    = (r, y, b, 0)
  | otherwise = (r, y - 20, b - 10, o)

-- Função para criar um caminho espiral a partir de uma linha
spiralRays :: Float -> Float -> Int -> Colour -> Line -> FractalTree.Picture
spiralRays angle scaleFactor n colour line
  = spiralRays' n colour line
  where
    spiralRays' n colour line@(p1, p2)
      | n <= 0 = []
      | otherwise = (colour, [p1, p2]) : spiralRays' (n-1) newColour newLine
      where
        newColour = if (n `mod` 3 == 0) then fade colour else colour
        newLine   = scaleLine scaleFactor (rotateLine angle line)

-- Função para criar um caminho espiral baseado no ângulo, fator de escala e número de iterações
spiral :: Float -> Float -> Int -> Line -> FractalTree.Path
spiral angle scaleFactor n line
  = spiralR' n line
  where
    spiralR' n line@(p1, p2)
      | n <= 0    = []
      | otherwise = p1 : spiralR' (n-1) newLine
      where
        newLine = connectLine line (scaleLine scaleFactor (rotateLine angle line))

-- Função para criar um polígono com n lados
polygon :: Int -> Line -> FractalTree.Path
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where 
    rotationAngle = (2 * pi) / (fromIntegral n)

-- Função que desenha o fractal de árvore Pitagórica
fractalTree :: Float -> Int -> Line -> FractalTree.Path
fractalTree factor n line = fractalTree' n line
  where
    fractalTree' 0 line = []  
    fractalTree' n line 
      = [p1, p4] ++ fractalTree' (n-1) (p4,p5) ++
                    fractalTree' (n-1) (p5,p3) ++
        [p3,p2] 
      where 
        flipLine (pS, pE) = (pE, pS)
        [p1,p2,p3,p4,_]   = FractalTree.polygon 4 line
        (_, p5)           = rotateLine (factor * pi) $ 
                              flipLine $ 
                                scaleLine 0.5 $ (p3, p4)

-- Função que desenha a árvore Pitagórica, com fundo preto e árvore verde
drawFractalTree :: GameState -> Graphics.Gloss.Picture
drawFractalTree gameState = Pictures
    [ Translate (0) (-100) $ Color black $ rectangleSolid 700 500
    , Color green $ Line (fractalTree (-pi/2) gameState ((0, -300), (100, -300)))  -- Desenha a árvore rotacionada
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)  -- Exibe o número de iterações
    , Translate (-300) 200 $ button (-1)                                             -- Botão de decremento
    , Translate 300 200 $ button 1                                                   -- Botão de incremento
    ]

-- Função que desenha o botão para incrementar/decrementar iterações
button :: Int -> Graphics.Gloss.Picture
button sign = Pictures 
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

-- Função para manipular os eventos do mouse
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -325 && x <= -275 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1
handleEvent _ gameState = gameState

-- Função para atualizar o estado do jogo
updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState
