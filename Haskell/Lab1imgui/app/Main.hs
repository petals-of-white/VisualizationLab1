{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Graphics.GL

import Control.Exception (bracket)
import Control.Exception.Base (bracket_)
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
import Render

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
  initGLFW
  runManaged $ do
    mwin <- makeWindow
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
          [gridVertShaderPath, gridFragShaderPath, graphVertShaderPath, graphFragShaderPath] <-
            sequence [gridVertShader, gridFragShader, graphVertShader, graphFragShader]
          gridShader <- prepareProgram gridVertShaderPath gridFragShaderPath
          graphShader <- prepareProgram graphVertShaderPath graphFragShaderPath
          gridVAO <- genObjectName
          graphVAO <- genObjectName
          graphVBO <- createVBO $ replicate 3000 0
          gridVBO <- createVBO $ concatMap (\Plot.Point {x = xx, y = yy} -> [xx, yy]) (plotGrid 10)
          bindVertexArrayObject $= Just gridVAO

          let vad = VertexArrayDescriptor 2 Float (fromIntegral $ sizeOf (0.0 :: Float) * 2) nullPtr
          linkAttrib gridVBO (AttribLocation 0) ToFloat vad
          bindVertexArrayObject $= Just graphVAO
          linkAttrib graphVBO (AttribLocation 0) ToFloat vad

          -- frame buffers
          frameBuf <- genObjectName :: IO FramebufferObject
          bindFramebuffer Framebuffer $= frameBuf

          -- window and viewport size
          let viewportW = 1024
              viewportH = 768

          -- texture creation, for we are going to render there
          renderedTexture <- genObjectName :: IO TextureObject
          textureBinding Texture2D $= Just renderedTexture
          texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D viewportW viewportH) 0 (PixelData RGB UnsignedByte nullPtr)
          textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
          framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D renderedTexture 0
          GL.drawBuffer $= FBOColorAttachment 0

          viewport $= (Position 0 0, Size viewportW viewportH)

          let glObjects = GLObjects gridVAO gridVBO gridShader graphVAO graphVBO graphShader renderedTexture

          font <- addGlobalStyles
          initState <- defaultState
          putStrLn "almost there!"
          mainLoop win font initState glObjects

          -- delete everything
          deleteObjectName frameBuf
          deleteObjectName renderedTexture
          deleteObjectNames [gridVAO, graphVAO]
          deleteObjectNames [gridVBO, graphVBO]
          deleteObjectNames [graphShader, gridShader]
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate