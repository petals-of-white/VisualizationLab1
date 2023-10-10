{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Graphics.GL

import Control.Exception (bracket)
import Control.Exception.Base (bracket_)
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui (createContext)
import qualified DearImGui.GLFW as ImguiGLFW
import qualified DearImGui.GLFW.OpenGL as ImguiGLFWGL
import qualified DearImGui.OpenGL3 as ImguiGL
import DearImGui.Raw (destroyContext)
import Foreign (Storable (sizeOf), nullPtr)
import GUI
import Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.UI.GLFW as GLFW
import Paths_Lab1imgui
import Plot
import GLHelpers
import Numeric.Natural (Natural)

gridVertShader :: IO FilePath
gridVertShader = getDataFileName "shaders/grid.vert"

gridFragShader :: IO FilePath
gridFragShader = getDataFileName "shaders/grid.frag"

graphVertShader :: IO FilePath
graphVertShader = getDataFileName "shaders/graph.vert"

graphFragShader :: IO FilePath
graphFragShader = getDataFileName "shaders/graph.frag"

main :: IO ()
main = do
  
  let glfwWindowSize@(Size imguiWinW imguiWinH) = Size 1920 1080
      snailPoints = 3000 :: Natural
      gridSize = 10 :: Natural
      -- window and viewport size
      canvasSize@(Size canvasW canvasH) = Size 300 300

  initGLFW
  runManaged $ do
    mwin <- makeWindow glfwWindowSize
    case mwin of
      Just win -> do
        liftIO $ do
          -- setup win
          GLFW.makeContextCurrent (Just win)
          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext
        
        -- not sure why we need it but okay
        _ <- managed_ $ bracket_ (ImguiGLFWGL.glfwInitForOpenGL win True) ImguiGLFW.glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ ImguiGL.openGL3Init ImguiGL.openGL3Shutdown
        liftIO $ do
          debugGL

          [gridVertShaderPath, gridFragShaderPath, graphVertShaderPath, graphFragShaderPath] <-
            sequence [gridVertShader, gridFragShader, graphVertShader, graphFragShader]

          let vad = VertexArrayDescriptor 2 Float (fromIntegral $ sizeOf (0.0 :: Float) * 2) nullPtr

          gridShader <- prepareProgram gridVertShaderPath gridFragShaderPath
          debugInfo 1 "Grid shader created"
          gridVAO <- genObjectName
          bindVertexArrayObject $= Just gridVAO
          gridVBO <- createVBO $ concatMap (\Plot.Point {x = xx, y = yy} -> [xx, yy]) (plotGrid gridSize)
          debugInfo 1 "Grid VBO created"
          linkAttrib gridVBO (AttribLocation 0) ToFloat vad

          debugInfo 1 "Grid stuff created"

          graphShader <- prepareProgram graphVertShaderPath graphFragShaderPath
          graphVAO <- genObjectName
          bindVertexArrayObject $= Just graphVAO

          graphVBO <- createVBO $ replicate (fromIntegral snailPoints) 0  
          linkAttrib graphVBO (AttribLocation 0) ToFloat vad

          debugInfo 1 "Grid stuff created"

          viewport $= (Position 0 0, canvasSize)

          -- frame buffers
          frameBuf <- genObjectName :: IO FramebufferObject
          bindFramebuffer Framebuffer $= frameBuf

          -- texture creation, for we are going to render there
          renderedTexture <- genObjectName :: IO TextureObject
          textureBinding Texture2D $= Just renderedTexture
          texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D canvasW canvasH) 0 (PixelData RGB UnsignedByte nullPtr)
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
          -- textureBinding Texture2D $= Nothing

          status <- framebufferStatus Framebuffer
          case status of
            Complete -> do
              GL.drawBuffer $= FBOColorAttachment 0
              bindFramebuffer Framebuffer $= defaultFramebufferObject
              let glObjects =
                    GLObjects
                      { gridVAO = gridVAO,
                        gridVBO = gridVBO,
                        gridShader = gridShader,
                        graphVAO = graphVAO,
                        graphVBO = graphVBO,
                        graphShader = graphShader,
                        targetTexture = renderedTexture,
                        frameBuffer = frameBuf
                      }
              font <- addGlobalStyles
              initState <- defaultState
              putStrLn "almost there!"
              clearColor $= Color4 0.8 0.8 0.8 1
              mainLoop win font initState glObjects snailPoints gridSize canvasSize glfwWindowSize
            _ -> error ""
          -- delete everything
          deleteObjectName frameBuf
          deleteObjectName renderedTexture
          deleteObjectNames [gridVAO, graphVAO]
          deleteObjectNames [gridVBO, graphVBO]
          deleteObjectNames [graphShader, gridShader]
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate