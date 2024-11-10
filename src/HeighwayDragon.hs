module HeighwayDragon (drawHeighwayDragon, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

type GameState = Int

-- Gera a curva do dragão de maneira acumulativa, sem concatenar listas
generateDragon :: Int -> [(Float, Float)]
generateDragon 0 = [(0, 0), (300, 0)]
generateDragon n = foldl extendDragon [(0, 0), (300, 0)] [1..n]
  where
    extendDragon :: [(Float, Float)] -> Int -> [(Float, Float)]
    extendDragon path _ =
      let lastPoint = last path
          rotatedPath = map (rotate90 lastPoint) (reverse path)
      in path ++ tail rotatedPath

-- Rotaciona um ponto 90 graus em relação a um ponto de referência
rotate90 :: (Float, Float) -> (Float, Float) -> (Float, Float)
rotate90 (cx, cy) (x, y) = (cx - (y - cy), cy + (x - cx))

drawHeighwayDragon :: GameState -> Picture
drawHeighwayDragon gameState = Pictures 
    [       Translate offsetX offsetY $ Scale scaleFactor scaleFactor $ Pictures
        [ Color (getColor i) (Line [path !! i, path !! (i + 1)]) | i <- [0..(length path - 2)]]
    ,      Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)
    , 
      Translate (-300) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]
  where
    scaleFactor = 1 / (2 ** (fromIntegral gameState / 2))
    offsetX = 0
    offsetY = 0
    
    path = generateDragon gameState

    getColor :: Int -> Color
    getColor i
      | i < length path `div` 2 = red  -- Vermelha para a primeira metade
      | otherwise = blue  -- Azul para a segunda metade
 

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
