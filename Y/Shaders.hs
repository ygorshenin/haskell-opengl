module Y.Shaders ( ShaderInfo (..)
                 , mkProgram
                 )
    where

import Control.Monad
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import qualified Graphics.Rendering.OpenGL as GL

data ShaderInfo = ShaderInfo FilePath GL.ShaderType deriving (Show)

loadCompileAndAttach :: GL.Program -> ShaderInfo -> IO ()
loadCompileAndAttach program sh@(ShaderInfo shPath shType) = do
  source <- BS.readFile shPath
  shader <- GL.createShader shType
  GL.shaderSourceBS shader $= source
  GL.compileShader shader
  ok <- GL.compileStatus shader
  unless ok $ do
    log <- GL.shaderInfoLog shader
    fail $ "Can't build " ++ (show sh) ++ ":" ++ log
  GL.attachShader program shader

mkProgram :: [ShaderInfo] -> IO GL.Program
mkProgram infos = do
  program <- GL.createProgram
  mapM_ (loadCompileAndAttach program) infos
  GL.linkProgram program
  ok <- GL.linkStatus program
  unless ok $ do
    log <- GL.programInfoLog program
    fail log
  shaders <- GL.get $ GL.attachedShaders program
  mapM_ (GL.detachShader program) shaders
  return program
