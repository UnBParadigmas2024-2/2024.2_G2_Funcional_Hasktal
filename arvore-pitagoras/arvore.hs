module FractalTree where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO
import Control.Monad (when)

windowWidth, windowHeight :: Int
windowWidth = 700
windowHeight = 800

data State = State { level :: Int, zoomLevel :: Float, angle :: Float, isActive :: Bool, maxSteps :: Int }

initialState :: Int -> State
initialState steps = State { level = 0, zoomLevel = 1.0, angle = 0, isActive = True, maxSteps = steps }

-- Função para ler a quantidade de passos do usuário
readSteps :: IO Int
readSteps = do
    putStr "Digite a quantidade de passos (0 a 7): "
    input <- getLine
    let steps = read input :: Int
    if steps < 0 || steps > 7
        then do
            putStrLn "Número inválido! Tente novamente."
            readSteps  
        else return steps

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  steps <- readSteps 
  putStrLn "Use as teclas + e - para ajustar o zoom, setas para rotacionar, e Esc para sair."
  play
    (InWindow "Fractal Tree" (windowWidth, windowHeight) (20, 20))
    black
    20 
    (initialState steps)  
    drawTree
    handleEvents
    updateTree

-- Função para desenhar a árvore com base no estado atual
drawTree :: State -> Picture
drawTree (State lvl z ang act maxLvl)
  | not act = Blank
  | otherwise = Scale z z $ Rotate ang $ Translate 0 (-300) (generateTree (min lvl maxLvl) brown)

handleEvents :: Event -> State -> State
handleEvents (EventKey (Char '+') Down _ _) state = state { zoomLevel = zoomLevel state * 1.1 }
handleEvents (EventKey (Char '-') Down _ _) state = state { zoomLevel = zoomLevel state / 1.1 }
handleEvents (EventKey (SpecialKey KeyLeft) Down _ _) state = state { angle = angle state - 5 }
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) state = state { angle = angle state + 5 }
handleEvents (EventKey (SpecialKey KeyEsc) Down _ _) state = state { isActive = False }
handleEvents _ state = state

updateTree :: Float -> State -> State
updateTree _ state
  | not (isActive state) = state
  | level state < maxSteps state = state { level = level state + 1 } 
  | otherwise = state  

drawTrunk :: Color -> Picture
drawTrunk color = Color color (Polygon [(30,0), (15,300), (-15,300), (-30,0)])

-- Função recursiva para desenhar a árvore
generateTree :: Int -> Color -> Picture
generateTree 0 color = drawTrunk color
generateTree n color = Pictures [drawTrunk color,
                                 Translate 0 300 smallerTree,
                                 Translate 0 240 (Rotate   20  smallerTree),
                                 Translate 0 180 (Rotate (-20) smallerTree),
                                 Translate 0 120 (Rotate   40  smallerTree),
                                 Translate 0  60 (Rotate (-40) smallerTree) ]
    where smallerTree = Scale 0.5 0.5 (generateTree (n-1) (increaseGreen color))

brown :: Color
brown = makeColor (139 / 255) (100 / 255) (35 / 255) 1

increaseGreen :: Color -> Color
increaseGreen color = mixColors 1.0 0.1 color green
