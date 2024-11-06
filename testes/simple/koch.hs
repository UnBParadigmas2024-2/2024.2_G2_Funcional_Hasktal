import Graphics.Gloss

-- Configurações iniciais: janela, cor de jundo, iterações e zoom inicial
window :: Display
window = InWindow "Zooming Koch Snowflake" (800, 800) (100, 100)
background :: Color
background = black
iterations :: Int
iterations = 5
initialZoom :: Float
initialZoom = 0.05

