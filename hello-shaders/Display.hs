module Display (display) where

import Control.Monad
import Data.IORef
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Y.Buffers
import Y.Context
import Y.Shaders

display :: IORef Context -> IO ()
display ctxRef = do
  let vertices = [ GL.Vertex2 (-0.5) 0.0
                 , GL.Vertex2 0.0 0.5
                 , GL.Vertex2 0.5 0.0
                 ] :: [GL.Vertex2 GL.GLfloat]
      numVertices = length vertices

  vao <- createVAO vertices
  program <- mkProgram [ ShaderInfo "shaders/triangle.vert" GL.VertexShader
                       , ShaderInfo "shaders/triangle.frag" GL.FragmentShader
                       ]

  let loop = do
      GLFW.pollEvents
      ctx <- readIORef ctxRef
      unless (ctxQuit ctx) $ do
          GL.clearColor $= GL.Color4 0.2 0.3 0.3 1.0
          GL.clear [GL.ColorBuffer]

          time <- GL.get GLFW.time

          GL.currentProgram $= Just program

          let triangleColor = GL.Color4 0.0 ((realToFrac $ sin time) * 0.5 + 0.5) 0.0 (1.0 :: GL.GLfloat)
          colorLocation <- GL.uniformLocation program "triangleColor"

          GL.uniform colorLocation $= triangleColor

          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 (fromIntegral $ length vertices)
          GL.currentProgram $= Nothing

          GLFW.swapBuffers
          display ctxRef
  loop
