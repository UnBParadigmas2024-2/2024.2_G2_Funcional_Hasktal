module Sierpinski (drawSierpinski, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))


type MyPoint = (Float, Float)
type Triangle = (MyPoint, MyPoint, MyPoint)
type GameState = Int

sierpinski :: Int -> Triangle -> [Triangle]
sierpinski 0 tri = [tri]
sierpinski n (p1, p2, p3) =
    sierpinski (n - 1) (p1, midPoint p1 p2, midPoint p1 p3) ++
    sierpinski (n - 1) (p2, midPoint p2 p3, midPoint p1 p2) ++
    sierpinski (n - 1) (p3, midPoint p1 p3, midPoint p2 p3)
  where
    midPoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

drawTriangle :: Int -> Triangle -> Picture
drawTriangle depth (p1, p2, p3) =
    let triColor = if depth == 0
        then makeColor 0 0 0 1
        else makeColor (fromIntegral depth / 7) (1 - fromIntegral depth / 7) 0.7 1
    in Color triColor (polygon [p1, p2, p3])

drawSierpinski :: GameState -> Picture
drawSierpinski gameState = Pictures 
    [ Pictures (map (drawTriangle gameState) (sierpinski gameState ((-350, -350), (350, -350), (0, 350))))
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)
    , Translate (-300) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]

button :: Int -> Picture
button sign = Pictures 
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -325 && x <= -275 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1
handleEvent _ gameState = gameState

updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState