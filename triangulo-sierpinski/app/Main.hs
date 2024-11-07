module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib (sierpinski, drawTriangle)

type GameState = Int

main :: IO ()
main = play (InWindow "Triangulo de Sierpinski" (800, 800) (450, 450)) 
            white 60 0 drawState handleEvent updateState

drawState :: GameState -> Picture
drawState gameState = Pictures 
    [ Pictures (map (drawTriangle gameState) (sierpinski gameState ((-350, -350), (350, -350), (0, 350))))
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)
    , Translate (-350) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]

button :: Int -> Picture
button sign = Pictures 
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1
handleEvent _ gameState = gameState

updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState
