module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Todas as telas do programa
data Screen = Menu | Mandelbrot | Julia | Sierpinski | Koch | ArvorePitagoras deriving (Eq, Show)

-- | Estado do app, contendo a tela atual
data AppState = AppState
  { currentScreen :: Screen
  }

-- | Estado inicial do app, começando com o Menu
initialState :: AppState
initialState = AppState { currentScreen = Menu }

main :: IO ()
main = play window white 30 initialState render handleInput update
  where
    window = InWindow "Hasktal" (800, 600) (100, 100)
    update _ = id

-- | Renderiza a tela com base no AppState
render :: AppState -> Picture
render state = case currentScreen state of
    Menu            -> renderMenu
    Mandelbrot      -> renderMandelbrot
    Julia           -> renderJulia
    Sierpinski      -> renderSierpinski
    Koch            -> renderKoch
    ArvorePitagoras -> renderArvorePitagoras

renderMenu :: Picture
renderMenu = Pictures
  [
    translate (-200) 100 $ scale 0.3 0.3 $ text "HASKTAL",
    translate (-200) (-50) $ scale 0.2 0.2 $ text "[1] Mandelbrot",
    translate (-200) (-100) $ scale 0.2 0.2 $ text "[2] Julia",
    translate (-200) (-150) $ scale 0.2 0.2 $ text "[3] Sierpinski",
    translate (-200) (-200) $ scale 0.2 0.2 $ text "[4] Koch",
    translate (-200) (-250) $ scale 0.2 0.2 $ text "[5] Arvore de Pitagoras"
  ]

renderMandelbrot :: Picture
renderMandelbrot = Pictures
    [ translate (-200) 250 $ scale 0.3 0.3 $ text "Mandelbrot"
    , translate (-200) (-50) $ scale 0.2 0.2 $ text "Digite 'm' para voltar"
    ]

renderJulia :: Picture
renderJulia = Pictures
    [ translate (-200) 250 $ scale 0.3 0.3 $ text "Julia"
    , translate (-200) (-50) $ scale 0.2 0.2 $ text "Digite 'm' para voltar"
    ]

renderSierpinski :: Picture
renderSierpinski = Pictures
    [ translate (-200) 250 $ scale 0.3 0.3 $ text "Sierpinski"
    , translate (-200) (-50) $ scale 0.2 0.2 $ text "Digite 'm' para voltar"
    ]

renderKoch :: Picture
renderKoch = Pictures
    [ translate (-200) 250 $ scale 0.3 0.3 $ text "Koch"
    , translate (-200) (-50) $ scale 0.2 0.2 $ text "Digite 'm' para voltar"
    ]

renderArvorePitagoras :: Picture
renderArvorePitagoras = Pictures
    [ translate (-200) 250 $ scale 0.3 0.3 $ text "Arvore de Pitagoras"
    , translate (-200) (-50) $ scale 0.2 0.2 $ text "Digite 'm' para voltar"
    ]

-- | Inputs do teclados para mudar de tela
handleInput :: Event -> AppState -> AppState
handleInput (EventKey (Char '1') Down _ _) state = state { currentScreen = Mandelbrot }
handleInput (EventKey (Char '2') Down _ _) state = state { currentScreen = Julia }
handleInput (EventKey (Char '3') Down _ _) state = state { currentScreen = Sierpinski }
handleInput (EventKey (Char '4') Down _ _) state = state { currentScreen = Koch }
handleInput (EventKey (Char '5') Down _ _) state = state { currentScreen = ArvorePitagoras }
handleInput (EventKey (Char 'm') Down _ _) state = state { currentScreen = Menu }
handleInput _ state = state
