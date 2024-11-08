import Graphics.Gloss

-- Configurações iniciais: janela, cor de jundo, iterações e zoom inicial
window :: Display
window = InWindow "Fractal de Koch" (800, 800) (100, 100)
background :: Color
background = black
iterations :: Int
iterations = 7
initialZoom :: Float
initialZoom = 0.05

main :: IO ()
main = animate window background (kochSnowflakeZooming iterations)

-- Função que define o floco de neve de Koch
kochSnowflakeZooming :: Int -> Float -> Picture
kochSnowflakeZooming maxIterations t =
  let
    -- Profundidade atual da recursão, baseada no tempo
    currentIterations = min maxIterations (floor t)
    
    -- Pontos de um triângulo equilátero
    p1 = (-200, -115)
    p2 = (200, -115)
    p3 = (0, 346)
    
    -- Gera os três lado do floco de neve
    path1 = kochFractal currentIterations p1 p2
    path2 = kochFractal currentIterations p2 p3
    path3 = kochFractal currentIterations p3 p1
    
    -- Adapata o fator de escala baseado na profundidade da recursão
    zoomFactor = initialZoom + t * 2
    
    -- Aplica o fator de zoom para os caminhos do fractal
    scaledPath1 = scalePath zoomFactor path1
    scaledPath2 = scalePath zoomFactor path2
    scaledPath3 = scalePath zoomFactor path3
  in
    color white (pictures [line scaledPath1, line scaledPath2, line scaledPath3])

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

-- Função para adaptar a escala ao fator de zoom
scalePath :: Float -> Path -> Path
scalePath zoomFactor = map (\(x, y) -> (x * zoomFactor, y * zoomFactor))