
module Main where


import Data.Char
import System.IO
import Data.List (intersperse, transpose)
import Data.Maybe

-- definindo os tipos de constante --> pensando na formacao das imgs
type Colour = Int
data RGB = RGB Colour Colour Colour deriving Show
type Bitmap = [[RGB]]

-- validando as cores do rgb
validColour :: Colour -> Bool
validColour colour = colour >= 0 && colour <= 255

validRGB :: RGB -> Bool
validRGB (RGB a b c) = all validColour [a, b, c]

-- cabecalho do arquivo ppm
ppmHeader :: (Int, Int) -> String
ppmHeader (a, b) = "P6 " ++ show a ++ " " ++ show b ++ " 255\n"

-- validando o bitmap
validBitmap :: Bitmap -> Maybe (Int, Int)
validBitmap [] = Just (0, 0)
validBitmap a@(xs : xss)
  | all (all validRGB) a && all (== length xs) (map length a) = Just (length xs, length a)
  | otherwise = Nothing

-- codificando uma cor RGB em string binaria
encodeRGB :: RGB -> String
encodeRGB (RGB a b c) = map chr [a, b, c]

--  codificando o bitmap inteiro
encodeBitmap :: Bitmap -> String
encodeBitmap = concatMap (concatMap encodeRGB)

-- salvando o arquivo PPM em modo binario
writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f x = do
  h <- openBinaryFile f WriteMode
  hPutStr h x
  hClose h

writePPM :: FilePath -> Bitmap -> IO ()
writePPM f x
  | validBitmap x == Nothing = error "The bitmap is not valid."
  | otherwise = writeBinaryFile f (ppmHeader (fromJust $ validBitmap x) ++ encodeBitmap x)

-- Fractais: Mandelbrot e Julia
type Point = (Float, Float)
type Fractal = Point -> [Point]

nextPoint :: Point -> Point -> Point
nextPoint (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)

mandelbrot :: Fractal
mandelbrot p = iterate (nextPoint p) (0, 0)

julia :: Point -> Fractal
julia c = iterate (nextPoint c)

-- renderizando os fractais
type Image = Point -> RGB

fairlyClose :: Point -> Bool
fairlyClose (a, b) = a * a + b * b < 100

fairlyCloseTill :: Int -> Fractal -> Point -> Int
fairlyCloseTill n f p = length (takeWhile fairlyClose (take n (f p)))

fracImage :: Fractal -> Image
fracImage f p = palette !! (fairlyCloseTill (length palette - 1) f p)

sample :: [[Point]] -> Image -> Bitmap
sample pss i = map (map i) pss

draw :: [[Point]] -> Fractal -> Bitmap
draw pss f = sample pss (fracImage f)

-- paleta de cores
type Palette = [RGB]

palette :: Palette
palette = take 15 (iterate f darkred) ++ replicate 5 green ++ [blue, black]
  where
    darkred = RGB 200 0 0
    f (RGB r g b) = RGB (r + 2) (g + 10) b

black, cyan, blue, green, magenta, yellow, red, white :: RGB
black = RGB 0 0 0
cyan = RGB 0 255 255
blue = RGB 0 0 255
green = RGB 0 255 0
magenta = RGB 255 0 255
yellow = RGB 255 255 0
red = RGB 255 0 0
white = RGB 255 255 255


size :: Int
size = 400

for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min + delta ..]
  where
    delta = (max - min) / fromIntegral (n - 1)

grid :: Int -> Int -> Point -> Point -> [[Point]]
grid c r (xmin, ymin) (xmax, ymax) =
  [[(x, y) | x <- for c xmin xmax] | y <- for r ymin ymax]

-- Exemplos de bitmaps
chessboard :: Bitmap
chessboard = concat $ alternate 8 evenRow oddRow
  where
    blackSquare = replicate 10 (replicate 10 black)
    whiteSquare = replicate 10 (replicate 10 white)
    evenRow = transpose $ concat $ alternate 8 whiteSquare blackSquare
    oddRow = transpose $ concat $ alternate 8 blackSquare whiteSquare
    alternate n x y
      | n == 0 = []
      | otherwise = x : alternate (n - 1) y x

gradient :: Bitmap
gradient = map (map distance) [[(x, y) | x <- [0 .. size - 1]] | y <- [0 .. size - 1]]
  where
    distance (x, y) =
      let step = round (fromIntegral (x + y) * 255 / 158)
       in RGB step step 255

figure1 :: Bitmap
figure1 = draw points mandelbrot
  where
    points = grid size size (-2.25, -1.5) (0.75, 1.5)

figure2 :: Bitmap
figure2 = draw points (julia (0.32, 0.043))
  where
    points = grid size size (-1.5, -1.5) (1.5, 1.5)

-- Funcao principal para salvar as imagens de fractais
main :: IO ()
main = do
  putStrLn "Writing mandelbrot bitmap..."
  writePPM "mandelbrot.ppm" figure1
  putStrLn "Writing julia bitmap..."
  writePPM "julia.ppm" figure2
  putStrLn "Done."
