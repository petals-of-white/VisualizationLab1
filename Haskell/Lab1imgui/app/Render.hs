module Render where

import Foreign (Storable (sizeOf), nullPtr, withArrayLen)
import Graphics.Rendering.OpenGL as GL
import Linear as Lin
import Numeric.Natural (Natural)
import Plot 
import Data.Foldable (Foldable(toList))

data GLObjects = GLObjects
  { gridVAO :: VertexArrayObject,
    gridVBO :: BufferObject,
    gridShader :: Program,
    graphVAO :: VertexArrayObject,
    graphVBO :: BufferObject,
    graphShader :: Program,
    targetTexture :: TextureObject
  }

transMatUniName :: String
transMatUniName = "u_transMatr"

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

drawPlot :: Natural -> GLObjects -> GLfloat -> GLfloat -> M44 GLfloat -> IO ()
drawPlot plotSize GLObjects {graphShader = graphShader, gridShader=gridShader, gridVAO=gridVAO, gridVBO=gridVBO, graphVAO=graphVAO, graphVBO=graphVBO} a l transMatrix = do
  GL.clear [ColorBuffer]

  -- FRAMEBUFFER INITIALIZATION
  frameBuf <- genObjectName :: IO FramebufferObject
  bindFramebuffer Framebuffer $= frameBuf

  -- CREATING TEXTURES
  renderedTexture <- genObjectName :: IO TextureObject
  textureBinding Texture2D $= Just renderedTexture
  texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D 1024 768) 0 (PixelData RGB UnsignedByte nullPtr)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
  GL.drawBuffer $= FBOColorAttachment 0
  viewport $= (Position 0 0, Size 1024 768)


  -- DRAW GRID
  bindVertexArrayObject $= Just gridVAO
  currentProgram $= Just gridShader
  transMatLoc <- get $ uniformLocation gridShader transMatUniName
  realMatrix <- newMatrix ColumnMajor $ concatMap toList (toList transMatrix) :: IO (GLmatrix Float)
  uniform transMatLoc $= realMatrix
  withArrayLen
    (concatMap (\Plot.Point{x=x,y=y} -> [x,y]) grid)
    ( \size arr ->
        let sizeInBytes = fromIntegral $ 2 * sizeOf (1.0 :: Float) * size
         in bufferData ArrayBuffer $= (sizeInBytes, arr, StaticDraw)
    )
  drawArrays Lines 0 40


  -- DRAW PLOT
  bindVertexArrayObject $= Just graphVAO
  currentProgram $= Just graphShader
  withArrayLen
    (concatMap (\Plot.Point{x=x,y=y} -> [x,y]) snail)
    ( \size arr ->
        let sizeInBytes = fromIntegral $ 2 * sizeOf (1.0 :: Float) * size
         in bufferData ArrayBuffer $= (sizeInBytes, arr, StaticDraw)
    )
  drawArrays Lines 0 3000

  where
    (snail, grid) = (plotPascalSnail a l plotSize, plotGrid 10 :: Plot Float)
