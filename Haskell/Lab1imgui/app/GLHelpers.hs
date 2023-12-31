module GLHelpers where

import Data.Foldable (Foldable (toList))
import Foreign (Storable (sizeOf), withArrayLen)
import Graphics.Rendering.OpenGL as GL
import Linear (M44)
import Numeric.Natural (Natural)
import Plot

data GLObjects = GLObjects
  { gridVAO :: VertexArrayObject,
    gridVBO :: BufferObject,
    gridShader :: Program,
    plotVAO :: VertexArrayObject,
    plotVBO :: BufferObject,
    plotShader :: Program,
    targetTexture :: TextureObject,
    frameBuffer :: FramebufferObject
  }

transMatUniName :: String
transMatUniName = "u_transform"

debugInfo :: GLuint -> String -> IO ()
debugInfo msgId msg =
  debugMessageInsert (DebugMessage DebugSourceApplication DebugTypeOther (DebugMessageID msgId) DebugSeverityNotification msg)

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
  validateProgram programID
  deleteObjectNames [vertShader, fragShader]
  return programID

linkAttrib :: BufferObject -> AttribLocation -> IntegerHandling -> VertexArrayDescriptor a -> IO ()
linkAttrib vbo location intHandling vaDescriptor = do
  bindBuffer ArrayBuffer $= Just vbo
  vertexAttribArray location $= Enabled
  vertexAttribPointer location $= (intHandling, vaDescriptor)
  bindBuffer ArrayBuffer $= Nothing

normalizedToGL :: Plot GLfloat -> Plot GLfloat
normalizedToGL plot = (\Plot.Point {x = xx, y = yy} -> Plot.Point {x = xx / maxX, y = yy / maxY}) <$> plot
  where
    maxX = maximum (abs . x <$> plot)
    maxY = maximum (abs . y <$> plot)

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
  bindBuffer ArrayBuffer $= Nothing
  return vbo

drawPlot :: Natural -> Natural -> GLObjects -> SnailOptions Float -> M44 GLfloat -> IO ()
drawPlot
  snailSize
  gridSize
  GLObjects
    { plotShader = plotShader,
      gridShader = gridShader,
      gridVAO = gridVAO,
      gridVBO = gridVBO,
      plotVAO = plotVAO,
      plotVBO = plotVBO
    }
  (SnailOptions a l)
  transMatrix = do
    GL.clear [ColorBuffer]
    debugInfo 10 $ show (snailSize, gridSize, plotShader, gridShader, gridVAO, gridVBO, plotVAO, plotVBO)
    realMatrix <- newMatrix RowMajor $ concatMap toList (toList transMatrix) :: IO (GLmatrix Float)

    -- DRAW GRID
    debugInfo 10 "Drawing grid..."
    bindVertexArrayObject $= Just gridVAO
    bindBuffer ArrayBuffer $= Just gridVBO
    currentProgram $= Just gridShader

    withArrayLen
      (concatMap (\Plot.Point {x = xx, y = yy} -> [xx, yy]) grid)
      ( \size arr ->
          let sizeInBytes = fromIntegral $ sizeOf (1.0 :: Float) * size
           in bufferData ArrayBuffer $= (sizeInBytes, arr, DynamicDraw)
      )

    gridTransMatLoc <- get $ uniformLocation gridShader transMatUniName
    uniform gridTransMatLoc $= realMatrix
    drawArrays Lines 0 (fromIntegral gridSize * 4)
    debugInfo 5 "grid drawn!"


    -- DRAW PLOT --
    bindVertexArrayObject $= Just plotVAO
    bindBuffer ArrayBuffer $= Just plotVBO
    currentProgram $= Just plotShader

    -- upload transofrm matrix
    plotTransMatLoc <- get $ uniformLocation plotShader transMatUniName
    uniform plotTransMatLoc $= realMatrix
    debugInfo 5 "Uniform uploaded"

    withArrayLen
      (concatMap (\Plot.Point {x = xx, y = yy} -> [xx, yy]) snail)
      ( \size arr ->
          let sizeInBytes = fromIntegral $ sizeOf (1.0 :: GLfloat) * size
           in bufferData ArrayBuffer $= (sizeInBytes, arr, DynamicDraw)
      )
    drawArrays LineStrip 0 (fromIntegral snailSize)
    debugInfo 6 "plot drawn"


    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing
    where
      snail = normalizedToGL $ plotPascalSnail a l snailSize
      grid = normalizedToGL $ plotGrid gridSize :: Plot GLfloat