{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Graphics.GL

import Control.Exception (bracket)
import Control.Exception.Base (bracket_)
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.IORef (newIORef)
import DearImGui (createContext, withFont)
import qualified DearImGui.GLFW as ImguiGLFW
import qualified DearImGui.GLFW.OpenGL as ImguiGLFWGL
import qualified DearImGui.OpenGL3 as ImguiGL
import DearImGui.Raw (destroyContext)
import GUI
import qualified Graphics.UI.GLFW as GLFW

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
          font <- addGlobalStyles
          putStrLn "Wow, Font? why doesn't it work?"
          initState <- defaultState
          mainLoop win font initState
      Nothing -> do
        error "GLFW createWindow failed"

  GLFW.terminate