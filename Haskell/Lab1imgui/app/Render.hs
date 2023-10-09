module Render where

import Foreign (Ptr, Storable (sizeOf), withArrayLen)
import Graphics.Rendering.OpenGL as GL
import Linear as Lin
import Numeric.Natural (Natural)
import Plot (plotGrid, plotPascalSnail)

prepareProgram :: FilePath -> FilePath -> IO Program
prepareProgram vertPath fragPath = do
  vertShader <- prepareShader VertexShader vertPath
  fragShader <- prepareShader FragmentShader fragPath
  programID <- createProgram
  attachShader programID vertShader
  attachShader programID fragShader
  linkProgram programID
  releaseShaderCompiler
  return programID

prepareShader :: ShaderType -> FilePath -> IO Shader
prepareShader shType path = do
  shaderText <- readFile path
  shader <- createShader shType
  shaderSourceBS shader $= packUtf8 shaderText
  compileShader shader
  return shader

linkAttrib :: BufferObject -> AttribLocation -> IntegerHandling -> VertexArrayDescriptor a -> IO ()
linkAttrib vbo location intHandling vaDescriptor = do
  bindBuffer ArrayBuffer $= Just vbo
  vertexAttribArray location $= Enabled
  vertexAttribPointer location $= (intHandling, vaDescriptor)
  bindBuffer ArrayBuffer $= Nothing

createVBO :: [GLfloat] -> IO BufferObject
createVBO vertices = do
  vbo <- genObjectName
  bindBuffer ArrayBuffer $= Just vbo
  withArrayLen
    vertices
    ( \size arr ->
        let sizeInBytes = fromIntegral $ sizeOf (head vertices) * size
         in bufferData ArrayBuffer $= (sizeInBytes, arr, StaticDraw)
    )
  return vbo

drawPlot :: Natural -> GLfloat -> GLfloat -> M44 GLfloat -> IO ()
drawPlot plotSize a l transMatrix = do
  GL.clearColor $= Color4 0 0 0 1
  
  GL.clear [ColorBuffer]
  undefined
  where
    (snail, grid) = (plotPascalSnail a l plotSize, plotGrid 10)
