module Lib (sierpinski, drawTriangle) where

import Graphics.Gloss

type MyPoint = (Float, Float)
type Triangle = (MyPoint, MyPoint, MyPoint)

sierpinski :: Int -> Triangle -> [Triangle]
sierpinski 0 tri = [tri]
sierpinski n (p1, p2, p3) =
    sierpinski (n - 1) (p1, midPoint p1 p2, midPoint p1 p3) ++
    sierpinski (n - 1) (p2, midPoint p2 p3, midPoint p1 p2) ++
    sierpinski (n - 1) (p3, midPoint p1 p3, midPoint p2 p3)
  where
    midPoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

drawTriangle :: Int -> Triangle -> Picture
drawTriangle depth (p1, p2, p3) =
    let triColor = if depth == 0
        then makeColor 0 0 0 1
        else makeColor (fromIntegral depth / 7) (1 - fromIntegral depth / 7) (0.7) 1
    in Color triColor (polygon [p1, p2, p3])
