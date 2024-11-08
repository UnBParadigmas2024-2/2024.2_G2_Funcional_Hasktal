module Koch (drawKoch, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))

type MyPoint = (Float, Float)
type LineSegment = (MyPoint, MyPoint)
type GameState = Int

-- Implementa o fractal de Koch de forma recursiva
-- Lembrando que os triângulos aqui são equiláteros
koch :: Int -> LineSegment -> [LineSegment]
koch 0 segment = [segment]
koch n ((x1, y1), (x2, y2)) =
    koch (n - 1) (p1, p2) ++
    koch (n - 1) (p2, p3) ++
    koch (n - 1) (p3, p4) ++
    koch (n - 1) (p4, p5)
  where
    p1 = (x1, y1)
    p5 = (x2, y2)
    dx = (x2 - x1) / 3
    dy = (y2 - y1) / 3
    p2 = (x1 + dx, y1 + dy)
    p4 = (x2 - dx, y2 - dy)
    midX = (x1 + x2) / 2
    midY = (y1 + y2) / 2
    heightX = midX - dy * sqrt 3 / 2
    heightY = midY + dx * sqrt 3 / 2
    p3 = (heightX, heightY)

-- Função que desenha um segmento de reta
drawSegment :: LineSegment -> Picture
drawSegment (p1, p2) = Color black (Line [p1, p2])

-- Desenha o fractal depois que foi calculado com a função koch
drawKoch :: GameState -> Picture
drawKoch gameState = Pictures 
    [ Pictures (map drawSegment (concatMap (koch gameState) [(start, end)]))
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iterations: " ++ show gameState)
    , Translate (-350) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]
  where
    start = (-350, -150)
    end = (350, -150)

-- Desenha os botões de aumentar e diminuir iterações
button :: Int -> Picture
button sign = Pictures 
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

-- Lida com os eventos jogo, aumentar ou diminuir iteração
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1
handleEvent _ gameState = gameState

updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState