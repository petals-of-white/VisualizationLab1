module Render where

import Foreign (Ptr, Storable (sizeOf), nullPtr, withArrayLen)
import Graphics.Rendering.OpenGL as GL
import Linear as Lin
import Numeric.Natural (Natural)
import Plot (plotGrid, plotPascalSnail)


prepareShader :: ShaderType -> FilePath -> IO Shader
prepareShader shType path = do
  shaderText <- readFile path
  shader <- createShader shType
  shaderSourceBS shader $= packUtf8 shaderText
  compileShader shader
  return shader

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
  frameBuf <- genObjectName :: IO FramebufferObject
  bindFramebuffer Framebuffer $= frameBuf
  renderedTexture <- genObjectName :: IO TextureObject
  textureBinding Texture2D $= Just renderedTexture
  texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D 1024 768) 0 (PixelData RGB UnsignedByte nullPtr)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
  GL.drawBuffer $= FBOColorAttachment 0
  viewport $= (Position 0 0, Size 1024 768) 
  GL.clear [ColorBuffer]
  transMatLoc <- get $ uniformLocation 
  uniform
  where
    (snail, grid) = (plotPascalSnail a l plotSize, plotGrid 10)
