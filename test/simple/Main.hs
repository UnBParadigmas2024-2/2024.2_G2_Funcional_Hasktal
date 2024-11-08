module Main where

import Graphics.UI.GLUT
import Control.Concurrent (threadDelay)
import Data.IORef
import Fractals (drawFractal)     -- Função de Sierpinski
import KochCurve (drawKochCurve)   -- Função de Curva de Koch
import Menu (FractalType(..), selectFractal)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Fractals Menu"
    windowSize $= Size 800 800
    fractalRef <- newIORef Sierpinski
    displayCallback $= display fractalRef
    keyboardCallback $= Just (keyboard fractalRef)
    mainLoop

display :: IORef FractalType -> IO ()
display fractalRef = do
    fractal <- readIORef fractalRef
    clear [ColorBuffer]
    case fractal of
        Sierpinski -> drawFractal (1, 1, -1, -1) 6
        KochCurve  -> drawKochCurve 5 (-0.9, 0.0) (0.9, 0.0)
    flush

keyboard :: IORef FractalType -> KeyboardCallback
keyboard fractalRef key _ = do
    fractal <- readIORef fractalRef
    let newFractal = selectFractal key fractal
    writeIORef fractalRef newFractal
    postRedisplay Nothing
