------------------------------
--  Author: Yao Zhang
------------------------------
module Fractals where

import Data.Char
import System.IO
import Data.List (intersperse, transpose)
import Data.Maybe 


validColour colour       = colour >= 0 && colour <=255

validRGB (RGB a b c)     = all validColour [a, b, c]

ppmHeader (a, b)         = "P6 " ++ show a ++ " " ++ show b ++ " 255\n"

validBitmap []           = Just(0, 0)                               
validBitmap a@(xs:xss)   | all (all validRGB) a && all (== length xs) (map length a)  = Just(length xs, length a) 
                         | otherwise                                                  = Nothing
                          
encodeRGB  (RGB a b c)   = chr a : chr b : chr c :[]

encodeBitmap             = concatMap (concatMap encodeRGB) 

writePPM  f x            | validBitmap x == Nothing = error "The bitmap is not valid."
                         | otherwise                = writeBinaryFile f ( (ppmHeader . fromJust . validBitmap) x ++ encodeBitmap x )

mandelbrot p             = iterate (nextPoint p) (0,0)

julia c                  = iterate (nextPoint c) 

sample pss i             = map (map i) pss

fairlyClose (a, b)       = a * a + b * b < 100

fairlyCloseTill n f p    = length (takeWhile fairlyClose (take n (f p)))
                            
fracImage f p            = palette !! (fairlyCloseTill (length palette - 1) f p)

draw pss f               = sample pss (fracImage f)


type Colour    = Int
data RGB       = RGB Colour Colour Colour
type Bitmap    = [[RGB]]

-------------------------------------------------------------------------------
--                             PART ONE: BITMAPS                             --
-------------------------------------------------------------------------------

validColour :: Colour -> Bool


validRGB :: RGB -> Bool


ppmHeader :: (Int,Int) -> String


validBitmap :: Bitmap -> Maybe (Int,Int)


encodeRGB :: RGB -> String


encodeBitmap :: Bitmap -> String


writeBinaryFile     :: FilePath -> String -> IO ()
writeBinaryFile f x = do
  h <- openBinaryFile f WriteMode
  hPutStr h x
  hClose h

writePPM :: FilePath -> Bitmap -> IO ()


-- Here are a few example bitmaps. You can use them to test the
-- "writePPM" function.
chessboard :: Bitmap
chessboard = concat $ alternate 8 evenRow oddRow
  where
  blackSquare   = replicate 10 (replicate 10 black)
  whiteSquare   = replicate 10 (replicate 10 white)
  evenRow       = transpose $ concat $ alternate 8 whiteSquare blackSquare
  oddRow        = transpose $ concat $ alternate 8 blackSquare whiteSquare
  alternate n x y
    | n == 0    = []
    | otherwise = x : alternate (n - 1) y x

gradient :: Bitmap
gradient =  map (map distance) [[(x,y) | x <- [0..size-1]] | y <- [0..size-1]]
  where
  size = 80
  distance (x,y) =
       let step = round (fromIntegral (x + y) * 255 / 158)
       in RGB step step 255


-------------------------------------------------------------------------------
--                             PART TWO: FRACTALS                            --
-------------------------------------------------------------------------------

type Point = (Float, Float)
type Fractal = Point -> [Point]

nextPoint             :: Point -> Point -> Point
nextPoint (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot :: Fractal


julia :: Point -> Fractal



-------------------------------------------------------------------------------
--                      PART THREE: RENDERING FRACTALS                       --
-------------------------------------------------------------------------------

type Image = Point -> RGB

sample :: [[Point]] -> Image -> Bitmap


fairlyClose :: Point -> Bool


fairlyCloseTill :: Int -> Fractal -> Point -> Int


fracImage :: Fractal -> Image


draw :: [[Point]] -> Fractal -> Bitmap


-- The colour palette
type Palette = [RGB]

palette :: Palette
palette = take 15 (iterate f darkred) ++ replicate 5 green ++ [blue, black]
  where
  darkred = RGB 200 0 0
  f (RGB r g b) = RGB (r + 2) (g + 10) (b)

black   = RGB 0 0 0
cyan    = RGB 0 255 255
blue    = RGB 0 0 255
green   = RGB 0 255 0
magenta = RGB 255 0 255
yellow  = RGB 255 255 0
red     = RGB 255 0 0
white   = RGB 255 255 255

-- Useful functions for computing bitmaps from an Image
size :: Int
size = 400

for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min + delta_ ..]
  where delta_ = (max-min) / fromIntegral (n-1)

grid :: Int -> Int -> Point -> Point -> [[Point]]
grid c r (xmin,ymin) (xmax,ymax) =
  [[ (x,y) | x <- for c xmin xmax ] | y <- for r ymin ymax ]

-- A few examples
figure1 :: Bitmap
figure1 = draw points mandelbrot
  where points = grid size size (-2.25, -1.5) (0.75, 1.5)

figure2 :: Bitmap
figure2 = draw points (julia (0.32,0.043))
  where points = grid size size (-1.5, -1.5) (1.5, 1.5)

main = do
  putStrLn "Writing mandelbrot bitmap..."
  writePPM "mandelbrot.ppm" figure1
  putStrLn "Writing julia bitmap..."
  writePPM "julia.ppm" figure2
  putStrLn "Done."