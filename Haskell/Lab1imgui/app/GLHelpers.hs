module GLHelpers where

import Data.Foldable (Foldable (toList))
import Foreign (Storable (sizeOf), withArrayLen)
import Graphics.Rendering.OpenGL as GL
  ( AttribLocation,
    BufferObject,
    BufferTarget (ArrayBuffer),
    BufferUsage (StaticDraw, DynamicDraw),
    Capability (Enabled),
    ClearBuffer (ColorBuffer),
    DebugMessage (DebugMessage),
    DebugMessageID (DebugMessageID),
    DebugSeverity (DebugSeverityNotification),
    DebugSource (DebugSourceApplication),
    DebugType (DebugTypeOther),
    FramebufferObject,
    GLfloat,
    GLmatrix,
    GLuint,
    GeneratableObjectName (genObjectName),
    HasGetter (get),
    HasSetter (($=)),
    IntegerHandling,
    Matrix (newMatrix),
    MatrixOrder (ColumnMajor, RowMajor),
    ObjectName (deleteObjectNames),
    PrimitiveMode (Lines, LineStrip),
    Program,
    Shader,
    ShaderType (FragmentShader, VertexShader),
    TextureObject,
    Uniform (uniform),
    VertexArrayDescriptor,
    VertexArrayObject,
    attachShader,
    bindBuffer,
    bindVertexArrayObject,
    bufferData,
    clear,
    compileShader,
    createProgram,
    createShader,
    currentProgram,
    debugMessageInsert,
    drawArrays,
    linkProgram,
    packUtf8,
    shaderSourceBS,
    uniformLocation,
    validateProgram,
    vertexAttribArray,
    vertexAttribPointer,
  )
import Linear as Lin
import Numeric.Natural (Natural)
import Plot

data GLObjects = GLObjects
  { gridVAO :: VertexArrayObject,
    gridVBO :: BufferObject,
    gridShader :: Program,
    graphVAO :: VertexArrayObject,
    graphVBO :: BufferObject,
    graphShader :: Program,
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
normalizedToGL plot = (\Point {x = xx, y = yy} -> Point {x = xx / maxX, y = yy / maxY}) <$> plot
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

drawPlot :: Natural -> Natural -> GLObjects -> GLfloat -> GLfloat -> M44 GLfloat -> IO ()
drawPlot
  snailSize
  gridSize
  GLObjects
    { graphShader = graphShader,
      gridShader = gridShader,
      gridVAO = gridVAO,
      gridVBO = gridVBO,
      graphVAO = graphVAO,
      graphVBO = graphVBO
    }
  a
  l
  transMatrix = do

    GL.clear [ColorBuffer]
    debugInfo 10 $ show (snailSize, gridSize, graphShader, gridShader, gridVAO, gridVBO, graphVAO, graphVBO)
    debugInfo 10 "Drawing grid..."
    -- DRAW GRID
    bindVertexArrayObject $= Just gridVAO
    bindBuffer ArrayBuffer $= Just gridVBO
    currentProgram $= Just gridShader

    withArrayLen
      (concatMap (\Plot.Point {x = xx, y = yy} -> [xx, yy]) grid)
      ( \size arr ->
          let sizeInBytes = fromIntegral $ sizeOf (1.0 :: Float) * size
           in bufferData ArrayBuffer $= (sizeInBytes, arr, DynamicDraw)
      )
    drawArrays Lines 0 (fromIntegral gridSize * 4)

    debugInfo 5 "grid drawn!"

    debugInfo 5 "drawing plot..."

    -- debugInfo 15 $ "Snail is : " ++ show snail

    -- DRAW PLOT --
    bindVertexArrayObject $= Just graphVAO
    bindBuffer ArrayBuffer $= Just graphVBO
    currentProgram $= Just graphShader

    -- upload transofrm matrix
    transMatLoc <- get $ uniformLocation graphShader transMatUniName
    realMatrix <- newMatrix ColumnMajor $ concatMap toList (toList transMatrix) :: IO (GLmatrix Float)

    uniform transMatLoc $= realMatrix

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