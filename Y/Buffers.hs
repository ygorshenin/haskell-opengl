module Y.Buffers ( listSize
                 , bufferOffset
                 , createVAO
                 )
    where

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

listSize :: Storable a => [a] -> GL.GLsizeiptr
listSize [] = 0
listSize xs = (fromIntegral $ length xs) * (fromIntegral . sizeOf $ head xs)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

createVAO vertices = do
  [vao] <- GL.genObjectNames 1
  [vbo] <- GL.genObjectNames 1
  -- Bind VAO, initialize and unbind.
  GL.bindVertexArrayObject $= Just vao
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  withArray vertices $ \ptr -> do
      GL.bufferData GL.ArrayBuffer $= (listSize vertices, ptr, GL.StaticDraw)

  let startIndex = 0
      location = GL.AttribLocation 0

  GL.vertexAttribPointer location $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (bufferOffset startIndex))
  GL.vertexAttribArray location $= GL.Enabled

  GL.bindVertexArrayObject $= Nothing
  return vao
