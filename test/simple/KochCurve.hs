module KochCurve where

import Graphics.UI.GLUT

drawKochCurve :: Int -> (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawKochCurve depth start end = do
    if depth == 0
        then drawLine start end
        else do
            let (x1, y1) = start
                (x2, y2) = end
                dx = (x2 - x1) / 3
                dy = (y2 - y1) / 3
                p1 = (x1 + dx, y1 + dy)
                p2 = (x1 + 2 * dx, y1 + 2 * dy)
                peak = ( (x1 + x2) / 2 + dy * (sqrt 3 / 2), (y1 + y2) / 2 - dx * (sqrt 3 / 2))

            drawKochCurve (depth - 1) start p1
            drawKochCurve (depth - 1) p1 peak
            drawKochCurve (depth - 1) peak p2
            drawKochCurve (depth - 1) p2 end

drawLine :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO ()
drawLine (x1, y1) (x2, y2) = renderPrimitive Lines $ do
    vertex $ Vertex2 x1 y1
    vertex $ Vertex2 x2 y2
