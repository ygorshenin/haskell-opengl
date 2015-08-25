module Main where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Context
import Display

keyPressed :: IORef Context -> GLFW.KeyCallback
keyPressed ctxRef (GLFW.SpecialKey GLFW.ESC) (GLFW.Press) = do
  modifyIORef ctxRef $ \ctx -> ctx { ctxShutdown = True }
keyPressed _ _ _ = return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow size@(GL.Size w h) = do
  GL.viewport $= (GL.Position 0 0, size)

main :: IO ()
main = do
  let windowWidth = 800
      windowHeight = 600
  GLFW.initialize
  GLFW.openWindowHint GLFW.OpenGLVersionMajor 3
  GLFW.openWindowHint GLFW.OpenGLVersionMinor 3
  GLFW.openWindowHint GLFW.OpenGLProfile GLFW.OpenGLCoreProfile
  ok <- GLFW.openWindow (GL.Size windowWidth windowHeight) [GLFW.DisplayAlphaBits 8] GLFW.Window
  unless ok $ fail "GLFW: can't create window."
      
  GLFW.windowTitle $= "OpenGL tutorial"
  ctxRef <- newIORef defaultContext
  GLFW.keyCallback $= keyPressed ctxRef
  GLFW.windowSizeCallback $= resizeWindow

  display ctxRef
  GLFW.terminate
