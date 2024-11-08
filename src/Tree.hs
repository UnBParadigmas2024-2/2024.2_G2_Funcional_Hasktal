module Tree (drawTree, handleEvent, updateState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), MouseButton(..), KeyState(..), MouseButton(..))

type GameState = Int

drawTrunk :: Color -> Picture
drawTrunk treeColor = Color treeColor (Polygon [(30,0), (15,300), (-15,300), (-30,0)])

generateTree :: Int -> Color -> Picture
generateTree 0 treeColor = drawTrunk treeColor
generateTree n treeColor = Pictures [drawTrunk treeColor,
                                 Translate 0 300 smallerTree,
                                 Translate 0 240 (Rotate   20  smallerTree),
                                 Translate 0 180 (Rotate (-20) smallerTree),
                                 Translate 0 120 (Rotate   40  smallerTree),
                                 Translate 0  60 (Rotate (-40) smallerTree) ]
    where smallerTree = Scale 0.5 0.5 (generateTree (n-1) (increaseGreen treeColor))

brown :: Color
brown = makeColor (139 / 255) (100 / 255) (35 / 255) 1

increaseGreen :: Color -> Color
increaseGreen treeColor = mixColors 1.0 0.1 treeColor green

drawTree :: GameState -> Picture
drawTree gameState = Pictures 
    [Translate 0 (-300) (generateTree gameState brown)
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
