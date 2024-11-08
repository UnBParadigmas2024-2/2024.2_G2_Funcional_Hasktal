module Fractals where

import Graphics.UI.GLUT

drawFractal :: (GLfloat, GLfloat, GLfloat, GLfloat) -> Int -> IO ()
drawFractal (top, right, bottom, left) 0 = drawTriangle (top, right, bottom, left)
drawFractal area iteration = subdivide area iteration

drawTriangle :: (GLfloat, GLfloat, GLfloat, GLfloat) -> IO ()
drawTriangle (top, right, bottom, left) = do
    let vertices = [Vertex2 left bottom, Vertex2 right bottom, Vertex2 ((left + right) / 2) top]
    renderPrimitive Triangles $ mapM_ vertex vertices

subdivide :: (GLfloat, GLfloat, GLfloat, GLfloat) -> Int -> IO ()
subdivide area iteration = do
    let iteration' = iteration - 1
        subAreas = subTriangles area
    mapM_ (\pos -> drawFractal pos iteration') subAreas

subTriangles :: (GLfloat, GLfloat, GLfloat, GLfloat) -> [(GLfloat, GLfloat, GLfloat, GLfloat)]
subTriangles (top, right, bottom, left) = 
    [ (top, (left + right) / 2, (top + bottom) / 2, left) 
    , ((top + bottom) / 2, (left + right) / 2, bottom, left)
    , ((top + bottom) / 2, right, bottom, (left + right) / 2)
    ]
