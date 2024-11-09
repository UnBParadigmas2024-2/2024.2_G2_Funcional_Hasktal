module HeighwayDragon (drawHeighwayDragon, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

type GameState = Int

-- Função recursiva para gerar a curva do dragão
generateDragon :: Int -> [(Float, Float)] -> [(Float, Float)]
generateDragon 0 path = path
generateDragon n path = generateDragon (n - 1) (path ++ newPath)
  where
    lastPoint = last path
    rotatedPath = map (rotate90 lastPoint) (reverse path)
    newPath = tail rotatedPath

-- Rotaciona um ponto 90 graus em relação a um ponto de referência
rotate90 :: (Float, Float) -> (Float, Float) -> (Float, Float)
rotate90 (cx, cy) (x, y) = (cx - (y - cy), cy + (x - cx))

-- Desenha o fractal da Curva do Dragão com base no estado do jogo (iterações)
drawHeighwayDragon :: GameState -> Picture
drawHeighwayDragon gameState = Pictures 
    [ -- Desenha o fractal com cores
      Translate offsetX offsetY $ Scale scaleFactor scaleFactor $ Pictures
        [ Color (getColor i) (Line [path !! i, path !! (i + 1)]) | i <- [0..(length path - 2)]]
    , -- Exibe o número de iterações
      Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show gameState)
    , -- Desenha os botões para incrementar e decrementar as iterações
      Translate (-350) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]
  where
    -- Ajuste do fator de escala com base nas iterações
    scaleFactor = 1 / (2 ** (fromIntegral gameState / 2))
    -- Posição centralizada do fractal
    offsetX = 0
    offsetY = 0
    
    -- Gera a curva do dragão
    path = generateDragon gameState [(0, 0), (300, 0)]

    -- Função que retorna a cor com base na iteração
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
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = max 0 (gameState - 1)
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 1 
handleEvent _ gameState = gameState

updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState
