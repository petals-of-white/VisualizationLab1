{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Control.Exception (bracket, bracket_)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Managed hiding (with)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import DearImGui as Imgui hiding (ImVec3 (..), ImVec4 (x, y))
import DearImGui.FontAtlas as Atlas
import DearImGui.GLFW as ImguiGLFW
import DearImGui.GLFW.OpenGL as ImguiGLFWGL
import qualified DearImGui.OpenGL3 as ImguiGL
import Foreign (WordPtr (WordPtr), castPtr, wordPtrToPtr)
import Foreign.Marshal (with)
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Linear (V3 (..))
import Plot (transformMatrix)
import Render
import Text.Read (readMaybe)

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

textToFloat :: Text -> Maybe Float
textToFloat = readMaybe . show

textToFloatDef :: Float -> Text -> Float
textToFloatDef def = fromMaybe def . textToFloat

-- textRefToFloat :: IORef Text -> IO
data SnailSettings = SnailSettings {a :: IORef Text, l :: IORef Text} deriving (Eq)

data Vector3S = Vector3S (IORef Text) (IORef Text) (IORef Text) deriving (Eq)

vector3StoVec3 :: Vector3S -> IO (V3 Float)
vector3StoVec3 (Vector3S xref yref zref) = do
  [xv, yv, zv] <- mapM readIORef [xref, yref, zref]
  return $ V3 (textToFloatDef 1 xv) (textToFloatDef 1 yv) (textToFloatDef 1 zv)

data AppState = AppState
  { snail :: SnailSettings,
    scaleV :: Vector3S,
    translateV :: Vector3S,
    rotateV :: Vector3S,
    rotDeg :: IORef Text
  }
  deriving (Eq)

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

mainLoop :: Window -> Font -> AppState -> GLObjects -> IO ()
mainLoop win font appState glObjects = do
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

          let (TextureObject texId) = targetTexture glObjects
              texPtr = castPtr $ wordPtrToPtr $ WordPtr $ fromIntegral texId
              siz = ImVec2 1024 768
              uv0 = ImVec2 0 0
              uv1 = ImVec2 1 1
              tint = ImVec4 1 1 1 1
              bg = tint

          let AppState
                { snail = SnailSettings {a = aText, l = lText},
                  scaleV = scaleVT,
                  translateV = translateVT,
                  rotateV = rotateVT,
                  rotDeg = rotDegT
                  -- scaleV = Vector3S scaleVTx scaleVTy scaleVTz,
                  -- translateV = Vector3S transVTx transVTy transVTz,
                  -- rotateV = Vector3S rotVTx rotVTy rotVTz
                } = appState
          [scV, transV, rotV] <- mapM vector3StoVec3 [scaleVT, translateVT, rotateVT]
          av <- textToFloatDef 1 <$> readIORef aText
          lv <- textToFloatDef 1 <$> readIORef lText
          rotD <- textToFloatDef 0 <$> readIORef rotDegT
          let transMat = transformMatrix scV transV rotV rotD

          drawPlot 3000 glObjects av lv transMat

          -- 
          with siz \szPtr ->
            with uv0 \uv0Ptr ->
              with uv1 \uv1Ptr ->
                with tint \tintPtr ->
                  with bg \bgPtr ->
                    image texPtr szPtr uv0Ptr uv1Ptr tintPtr bgPtr
          

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

    withFont font do
      body
      showDemoWindow
      showAboutWindow
      showUserGuide
      showMetricsWindow

    render
    ImguiGL.openGL3RenderDrawData =<< getDrawData
    GLFW.swapBuffers win
    mainLoop win font appState glObjects