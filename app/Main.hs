module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))
import Sierpinski (drawSierpinski)
import Koch (drawKoch)
import Mandelbrot(drawMandelbrot)
import PythagorasTree (drawPythagorasTree)

data GameState = StartScreen | SierpinskiScreen Int | KochScreen Int | PythagorasTreeScreen Int | MandelbrotScreen Int

main :: IO ()
main = play 
    (InWindow "Hasktal" (800, 800) (450, 450)) 
    white 
    60 
    StartScreen 
    drawState 
    handleMainEvent 
    updateMainState

-- Desenha os botões de cada fractal no menu inicial
drawState :: GameState -> Picture
drawState StartScreen = Pictures
    [ Translate (-50) 200 $ Scale 0.3 0.3 $ Text "HASKTAL"
    , Translate (-50) (-50) $ Scale 0.2 0.2 $ Text "Sierpinski"
    , Translate (-50) (-100) $ Scale 0.2 0.2 $ Text "Koch"
    , Translate (-50) (-150) $ Scale 0.2 0.2 $ Text "Mandelbrot"
    , Translate (-50) (-200) $ Scale 0.2 0.2 $ Text "Pythagoras Tree"
    ]

-- Desenha o botão de voltar na tela de cada fractal
drawState (SierpinskiScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawSierpinski iterations
        ]
drawState (KochScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawKoch iterations
        ]
drawState (MandelbrotScreen iterations) = 
    Pictures
        [ Translate (-350) 325 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawMandelbrot iterations
        ]
drawState (PythagorasTreeScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawPythagorasTree iterations
        ]

-- Checa se cada botão do menu inicial foi pressionado
handleMainEvent :: Event -> GameState -> GameState
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) StartScreen
    | x >= (-50) && x <= 150 && y >= (-50) && y <= 0 = SierpinskiScreen 0
    | x >= (-50) && x <= 150 && y >= (-100) && y <= (-50) = KochScreen 0
    | x >= (-50) && x <= 150 && y >= (-150) && y <= (-50) = MandelbrotScreen 0
    | x >= (-50) && x <= 150 && y >= (-200) && y <= (-50) = PythagorasTreeScreen 0

-- Para cada tela de fractal, verifica que os botões de
-- aumentar iteração, diminuir iteração ou voltar foram apertados    
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (SierpinskiScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = SierpinskiScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = SierpinskiScreen (iterations + 1)
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (KochScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = KochScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = KochScreen (iterations + 1)
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (MandelbrotScreen iterations)
    | x >= (-325) && x <= (-150) && y >= 325 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = MandelbrotScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = MandelbrotScreen (iterations + 1)
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (PythagorasTreeScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = PythagorasTreeScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = PythagorasTreeScreen (iterations + 1)
handleMainEvent _ state = state

updateMainState :: Float -> GameState -> GameState
updateMainState _ state = state