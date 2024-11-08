module Menu where

data FractalType = Sierpinski | KochCurve
    deriving (Show, Eq)

selectFractal :: Char -> FractalType -> FractalType
selectFractal 's' _ = Sierpinski
selectFractal 'k' _ = KochCurve
selectFractal _ fractal = fractal  -- Mantém o fractal atual se a tecla não for reconhecida
