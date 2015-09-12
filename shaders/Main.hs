module Main where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Context
import Display

keyPressed :: IORef Context -> GLFW.Key -> GLFW.KeyButtonState -> IO ()
keyPressed ctxRef (GLFW.SpecialKey GLFW.ESC) (GLFW.Press) = do
  ctx <- readIORef ctxRef
  writeIORef ctxRef $ ctx { ctxQuit = True }
keyPressed _ _ _ = return ()

main :: IO ()
main = do
  let windowSize = GL.Size 800 600
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 2
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile

  ok <- GLFW.openWindow windowSize [GLFW.DisplayAlphaBits 8] GLFW.Window
  unless ok $ fail "GLFW: can't open window."

  ctxRef <- newIORef defaultContext
  GLFW.keyCallback $= keyPressed ctxRef

  display ctxRef
  GLFW.terminate
