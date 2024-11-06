module Lib (sierpinski, drawTriangle) where

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

