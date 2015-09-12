module Display (display) where

import Control.Monad
import Data.IORef
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Context
import Y.Buffers
import Y.Shaders

vertices :: [GL.Vertex2 GL.GLfloat]
vertices = [ GL.Vertex2 0.5 0.5
           , GL.Vertex2 (-0.5) (-0.5)
           , GL.Vertex2 (-0.5) 0.5
           , GL.Vertex2 0.2 0
           , GL.Vertex2 0 (-0.2)
           , GL.Vertex2 0 0
           ]

display :: IORef Context -> IO ()
display ctxRef = do
  vao1 <- createVAO (take 3 vertices)
  vao2 <- createVAO (drop 3 vertices)

  program1 <- mkProgram [ ShaderInfo "shaders/triangle.vert" GL.VertexShader
                        , ShaderInfo "shaders/triangle-orange.frag" GL.FragmentShader
                        ]
  program2 <- mkProgram [ ShaderInfo "shaders/triangle.vert" GL.VertexShader
                        , ShaderInfo "shaders/triangle-yellow.frag" GL.FragmentShader
                        ]

  let loop = do
      GLFW.pollEvents
      ctx <- readIORef ctxRef
      unless (ctxShutdown ctx) $ do
          GL.clearColor $= GL.Color4 0.2 0.3 0.3 1
          GL.clear [GL.ColorBuffer]

          forM_ [(vao1, program1), (vao2, program2)] $ \(vao, program) -> do
               GL.currentProgram $= Just program
               GL.bindVertexArrayObject $= Just vao
               GL.drawArrays GL.Triangles 0 3
          GLFW.swapBuffers
          loop
  loop
