module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..))
import HeighwayDragon (drawHeighwayDragon)
import Julia (drawJulia)
import Koch (drawKoch)
import Mandelbrot(drawMandelbrot)
import PythagorasTree (drawPythagorasTree)
import Sierpinski (drawSierpinski)
import SierpinskiCarpet (drawSierpinskiCarpet)

data GameState = StartScreen 
               | HeighwayDragonScreen Int 
               | JuliaScreen Int 
               | KochScreen Int 
               | MandelbrotScreen Int 
               | PythagorasTreeScreen Int 
               | SierpinskiScreen Int 
               | SierpinskiCarpetScreen Int 

main :: IO ()
main = play 
    (InWindow "Hasktal" (800, 800) (450, 450))  -- Cria uma janela de 800x800 pixels com título "Hasktal".
    white                                        -- Define a cor de fundo como branco.
    60                                           -- Define 60 quadros por segundo.
    StartScreen                                  -- Define o estado inicial como a tela inicial.
    drawState                                    -- Função para desenhar o estado atual na tela.
    handleMainEvent                              -- Função para manipular eventos do usuário (cliques).
    updateMainState                              -- Função para atualizar o estado do jogo.

-- Desenha os botões de cada fractal no menu inicial
drawState :: GameState -> Picture
drawState StartScreen = Pictures
    [ Translate (-50) 200 $ Scale 0.3 0.3 $ Text "HASKTAL"
    , Translate (-50) (-50) $ Scale 0.2 0.2 $ Text "Heighway Dragon"
    , Translate (-50) (-100) $ Scale 0.2 0.2 $ Text "Julia"
    , Translate (-50) (-150) $ Scale 0.2 0.2 $ Text "Koch"
    , Translate (-50) (-200) $ Scale 0.2 0.2 $ Text "Mandelbrot"
    , Translate (-50) (-250) $ Scale 0.2 0.2 $ Text "Pythagoras Tree"
    , Translate (-50) (-300) $ Scale 0.2 0.2 $ Text "Sierpinski"
    , Translate (-50) (-350) $ Scale 0.2 0.2 $ Text "Sierpinski Carpet"
    ]

-- Desenha o botão de voltar na tela de cada fractal
-- HeighwayDragon
drawState (HeighwayDragonScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawHeighwayDragon iterations
        ]
-- Julia
drawState (JuliaScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawJulia iterations
        ]
-- Koch
drawState (KochScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawKoch iterations
        ]
-- Mandelbrot
drawState (MandelbrotScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawMandelbrot iterations
        ]
-- PythagorasTree
drawState (PythagorasTreeScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawPythagorasTree iterations
        ]
-- Sierpinski
drawState (SierpinskiScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawSierpinski iterations
        ]
-- SierpinskiCarpet
drawState (SierpinskiCarpetScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawSierpinskiCarpet iterations
        ]

-- Checa se cada botão do menu inicial foi pressionado
handleMainEvent :: Event -> GameState -> GameState
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) StartScreen
    | x >= (-50) && x <= 150 && y >= (-50) && y <= 0 = HeighwayDragonScreen 0
    | x >= (-50) && x <= 150 && y >= (-100) && y <= (-50) = JuliaScreen 0
    | x >= (-50) && x <= 150 && y >= (-150) && y <= (-100) = KochScreen 0
    | x >= (-50) && x <= 150 && y >= (-200) && y <=  (-150) = MandelbrotScreen 0
    | x >= (-50) && x <= 150 && y >= (-250) && y <= (-200) = PythagorasTreeScreen 0
    | x >= (-50) && x <= 150 && y >= (-300) && y <= (-250) = SierpinskiScreen 0
    | x >= (-50) && x <= 150 && y >= (-350) && y <= (-300) = SierpinskiCarpetScreen 0

-- Para cada tela de fractal, verifica que os botões de
-- aumentar iteração, diminuir iteração ou voltar foram apertados    
-- HeighwayDragon
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (HeighwayDragonScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen                            -- Botão "Voltar".
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = HeighwayDragonScreen (max 0 (iterations - 1))  -- Diminui iterações.
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = HeighwayDragonScreen (iterations + 1)            -- Aumenta iterações.
-- Julia
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (JuliaScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = JuliaScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = JuliaScreen (iterations + 1)
-- Koch
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (KochScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = KochScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = KochScreen (iterations + 1)
-- Mandelbrot
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (MandelbrotScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = MandelbrotScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = MandelbrotScreen (iterations + 1)
-- PythagorasTree
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (PythagorasTreeScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = PythagorasTreeScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = PythagorasTreeScreen (iterations + 1)
-- SierpinskiCarpet
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (SierpinskiCarpetScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = SierpinskiCarpetScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = SierpinskiCarpetScreen (iterations + 1)
-- Sierpinski
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (SierpinskiScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = SierpinskiScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = SierpinskiScreen (iterations + 1)
-- Qualquer outro evento não altera o estado atual.
handleMainEvent _ state = state

-- Atualiza o estado (neste caso, nada muda com o tempo, então o estado é mantido).
updateMainState :: Float -> GameState -> GameState
updateMainState _ state = state
