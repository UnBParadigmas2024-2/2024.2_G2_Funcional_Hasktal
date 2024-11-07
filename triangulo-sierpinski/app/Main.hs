module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))
import Sierpinski (drawSierpinski)

data GameState = StartScreen | SierpinskiScreen Int -- Alterado para armazenar o estado do Sierpinski

main :: IO ()
main = play (InWindow "Hasktal" (800, 800) (450, 450)) 
            white 60 (StartScreen) drawState handleMainEvent updateMainState

drawState :: GameState -> Picture
drawState (StartScreen) = Pictures
    [ Translate (-50) 200 $ Scale 0.3 0.3 $ Text "Hasktal"
    , Translate (-50) (-50) $ Scale 0.2 0.2 $ Text "Sierpinski"
    ]
drawState (SierpinskiScreen iterations) = 
    Pictures
        [ Translate (-350) 350 $ Scale 0.2 0.2 $ Text "Voltar"
        , drawSierpinski iterations
        ]

handleMainEvent :: Event -> GameState -> GameState
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (StartScreen)
    | x >= (-50) && x <= 150 && y >= (-50) && y <= 50 = SierpinskiScreen 0
handleMainEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (SierpinskiScreen iterations)
    | x >= (-350) && x <= (-150) && y >= 350 && y <= 400 = StartScreen
    | x >= -375 && x <= -325 && y >= 175 && y <= 225 = SierpinskiScreen (max 0 (iterations - 1))
    | x >= 275 && x <= 325 && y >= 175 && y <= 225 = SierpinskiScreen (iterations + 1)
handleMainEvent _ state = state

updateMainState :: Float -> GameState -> GameState
updateMainState _ state = state
