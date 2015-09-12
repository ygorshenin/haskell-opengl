module Display (display) where

import Control.Monad
import Data.IORef
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Y.Buffers
import Y.Context
import Y.Shaders

vertices = [ 0.5, (-0.5),    1.0, 0.0, 0.0
           , (-0.5), (-0.5), 0.0, 1.0, 0.0
           , 0.0, 0.5,        0.0, 0.0, 1.0
           ] :: [GL.GLfloat]

display :: IORef Context -> IO ()
display ctxRef = do
  [vao] <- GL.genObjectNames 1
  [vbo] <- GL.genObjectNames 1

  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  withArray vertices $ \ptr -> do
      GL.bufferData GL.ArrayBuffer $= (listSize vertices, ptr, GL.StaticDraw)
  GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 20 (bufferOffset 0))
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 20 (bufferOffset 8))
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

  GL.bindVertexArrayObject $= Nothing

  let shaders = [ ShaderInfo "shaders/triangle.vert" GL.VertexShader
                , ShaderInfo "shaders/triangle.frag" GL.FragmentShader
                ]
  program <- mkProgram shaders
  GL.currentProgram $= Just program

  let loop = do
      GLFW.pollEvents
      ctx <- readIORef ctxRef
      unless (ctxQuit ctx) $ do
          GL.clearColor $= GL.Color4 0.2 0.3 0.3 1
          GL.clear [GL.ColorBuffer]
          GL.bindVertexArrayObject $= Just vao
          GL.drawArrays GL.Triangles 0 3
          GLFW.swapBuffers
          loop
  loop

  GL.currentProgram $= Nothing
