module Julia (julia, drawJulia, button, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))

type GameState = Int

-- Função para calcular o número de iterações antes de divergir
julia :: Int -> (Float, Float) -> (Float, Float) -> Int
julia maxIter (cx, cy) (x0, y0) = length . takeWhile (\(x, y) -> x*x + y*y <= 10) . take maxIter $ iterate f (x0, y0)
  where
    f (x, y) = (x*x - y*y + cx, 2*x*y + cy)


-- Função para desenhar o conjunto de Mandelbrot
drawJulia :: GameState -> Picture
drawJulia maxIter = Pictures
    [ Translate (-200) (-400) $ Scale 2.5 2.5 $ Pictures [translate x y $ drawPoint (julia maxIter c (sx x, sy y)) | x <- [-250, -248..250], y <- [-250, -248..250]]
    , Translate (-300) 300 $ Scale 0.2 0.2 $ Text ("Iteracoes: " ++ show maxIter)
    , Translate (-300) 200 $ button (-1)
    , Translate 300 200 $ button 1
    ]
  where
    c = (0.32, 0.043)
    sx x = x / 50 - 2  -- Ajuste para coordenada x
    sy y = y / 50 - 2  -- Ajuste para coordenada y

-- Função para desenhar um ponto do Mandelbrot de acordo com a quantidade de iterações
drawPoint :: Int -> Picture
drawPoint iter
    | iter == 100 = Color black $ rectangleSolid 4 4
    | otherwise   = Color (makeColorI (iter * 5 `mod` 255) (iter * 2 `mod` 255) (255 - iter * 10 `mod` 255) 255) $ rectangleSolid 4 4

button :: Int -> Picture
button sign = Pictures
    [ Color black $ rectangleSolid 50 50
    , Color white $ Translate (-10) (-10) $ Scale 0.2 0.2 $ Text (if sign == 1 then "+" else "-")
    ]

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gameState
    | x >= -325 && x <= -275 && y >= 175 && y <= 225 = max 0 (gameState - 20)  -- Diminui iterações com botão esquerdo
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = gameState + 20             -- Aumenta iterações com botão direito
handleEvent _ gameState = gameState

updateState :: Float -> GameState -> GameState
updateState _ gameState = gameState