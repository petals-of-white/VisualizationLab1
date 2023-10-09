{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Control.Exception (assert, bracket, bracket_, try)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Managed
-- import Data.Foldable (for_)

import Data.IORef
import Data.List (nub)
import Data.Text (Text)
import DearImGui as Imgui hiding (ImVec3 (..), ImVec4 (..))
import DearImGui.FontAtlas as Atlas
import DearImGui.GLFW as ImguiGLFW
import DearImGui.GLFW.OpenGL as ImguiGLFWGL
import qualified DearImGui.OpenGL3 as ImguiGL
import Foreign (malloc)
import GHC.IO.Exception (assertError)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW

-- import DearImGui.Raw.Font.GlyphRanges(Builtin(Cyrillic))
makeWindow :: Managed (Maybe Window)
makeWindow = managed $ bracket (GLFW.createWindow 800 600 "Hello, Dear ImGui!" Nothing Nothing) (maybe (return ()) GLFW.destroyWindow)

initGLFW :: IO ()
initGLFW = do
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "GLFW init failed"

addGlobalStyles :: IO Font
addGlobalStyles = do
  styleColorsClassic
  pushStyleVar ImGuiStyleVar_WindowPadding (return $ Imgui.ImVec2 {x = 40, y = 40} :: IO Imgui.ImVec2)
  let fontSrc = FromTTF "C:\\Windows\\Fonts\\arial.ttf" 22 Nothing Latin
  fonts <- rebuild $ [fontSrc, Atlas.DefaultFont]
  return $ head fonts

-- where
-- csOptions = mconcat [fontNo 0, glyphOffset (0, -1)]

setup :: Window -> IO ()
setup win = runManaged $ do
  liftIO $ do
    GLFW.makeContextCurrent (Just win)
    GLFW.swapInterval 1

  -- Create an ImGui context
  _ <- managed $ bracket createContext destroyContext

  -- not sure why we need it but okay
  _ <- managed_ $ bracket_ (ImguiGLFWGL.glfwInitForOpenGL win True) ImguiGLFW.glfwShutdown

  -- Initialize ImGui's OpenGL backend
  _ <- managed_ $ bracket_ ImguiGL.openGL3Init ImguiGL.openGL3Shutdown
  return ()

data SnailSettings = SnailSettings {a :: IORef Text, l :: IORef Text} deriving (Eq)

data Vector3S = Vector3S (IORef Text) (IORef Text) (IORef Text) deriving (Eq)

data AppState = AppState {snail :: SnailSettings, scaleV :: Vector3S, translateV :: Vector3S, rotateV :: Vector3S, rotDeg :: IORef Text} deriving (Eq)

valueInput :: (MonadIO m) => Text -> IORef Text -> m Bool
valueInput label ref = inputText label ref 6

defaultState :: IO AppState
defaultState = do
  [aa, ll, scaleX, scaleY, scaleZ] <- replicateM 5 $ newIORef "1"
  [trX, trY, trZ, rotX, rotY, rotZ, rotD] <- replicateM 7 $ newIORef "0"
  pure
    AppState
      { snail = SnailSettings {a = aa, l = ll},
        scaleV = Vector3S scaleX scaleY scaleZ,
        translateV = Vector3S trX trY trZ,
        rotateV = Vector3S rotX rotY rotZ,
        rotDeg = rotD
      }

mainLoop :: Window -> Font -> AppState -> IO ()
mainLoop win font appState = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close do
    -- Tell ImGui we're starting a new frame
    ImguiGL.openGL3NewFrame
    ImguiGLFW.glfwNewFrame
    Imgui.newFrame

    setNextWindowSize (return $ ImVec2 {x = 600, y = 600} :: IO ImVec2) ImGuiCond_Once
    -- Build the GUI
    let body = withWindowOpen "Close your eyees" do
          -- Add a text widget
          text "Pascal Snail"
          _ <- inputText "a" (a $ snail appState) 3
          _ <- inputText "l" (l $ snail appState) 3

          text "scale vector"
          let (Vector3S sx sy sz) = scaleV appState
          _ <- valueInput "sx" sx
          _ <- valueInput "sy" sy
          _ <- valueInput "sz" sz

          text "translation vector"
          let (Vector3S tx ty tz) = translateV appState
          _ <- valueInput "tx" tx
          _ <- valueInput "ty" ty
          _ <- valueInput "tz" tz

          text "rotation vector"
          let (Vector3S rx ry rz) = rotateV appState
          _ <- valueInput "rx" rx
          _ <- valueInput "ry" ry
          _ <- valueInput "rz" rz
          _ <- valueInput "Deg" (rotDeg appState)

          -- when inputing $ putStrLn "inputing!"
          -- Add a button widget, and call 'putStrLn' when it's clicked
          clicking <- button "Clickety Click"

          when clicking $
            putStrLn "Ow!"

          itemContextPopup do
            text "pop!"
            button "ok" >>= \clicked ->
              when clicked $
                closeCurrentPopup

          newLine

    -- Render
    GL.clear [GL.ColorBuffer]

    withFont font body
    showDemoWindow
    showAboutWindow
    showUserGuide
    showMetricsWindow

    render
    ImguiGL.openGL3RenderDrawData =<< getDrawData
    GLFW.swapBuffers win
    mainLoop win font appState