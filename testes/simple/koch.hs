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

-- Função que gera os pontos de um lado do fractal
kochFractal :: Int -> Point -> Point -> Path
kochFractal 0 p1 p2 = [p1, p2]
kochFractal n (x1, y1) (x2, y2) =
  let
    -- Pontos necessários para o "bump" do fractal
    dx = (x2 - x1) / 3
    dy = (y2 - y1) / 3
    pA = (x1 + dx, y1 + dy)
    pB = (x2 - dx, y2 - dy)
    angle = pi / 3
    pPeak = (x1 + dx + cos angle * dx - sin angle * dy,
             y1 + dy + sin angle * dx + cos angle * dy)
  in
    -- Gera recursivamente os pontos para cada segmento
    kochFractal (n - 1) (x1, y1) pA ++
    kochFractal (n - 1) pA pPeak ++
    kochFractal (n - 1) pPeak pB ++
    kochFractal (n - 1) pB (x2, y2)