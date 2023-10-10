{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Control.Exception (bracket, bracket_, throwIO)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Managed hiding (with)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import DearImGui as Imgui hiding (ImVec3 (..), ImVec4 (x, y))
import DearImGui.FontAtlas as Atlas
import DearImGui.GLFW as ImguiGLFW
import qualified DearImGui.OpenGL3 as ImguiGL
import Foreign (WordPtr (WordPtr), castPtr, wordPtrToPtr)
import Foreign.Marshal (with)
import GHC.Natural (Natural)
import GLHelpers
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Linear (V3 (..))
import Plot (transformMatrix)
import Text.Read (readMaybe)

-- import DearImGui.Raw.Font.GlyphRanges(Builtin(Cyrillic))
makeWindow :: Size -> Managed (Maybe Window)
makeWindow (Size width height) =
  managed $
    bracket
      (GLFW.createWindow (fromIntegral width) (fromIntegral height) "Равлик паскаля" Nothing Nothing)
      (maybe (return ()) GLFW.destroyWindow)

initGLFW :: IO ()
initGLFW = do
  setErrorCallback (Just \err msg -> putStrLn $ show err ++ " " ++ msg)
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "GLFW init failed"

addGlobalStyles :: IO Font
addGlobalStyles = do
  styleColorsClassic
  pushStyleVar ImGuiStyleVar_WindowPadding (return $ Imgui.ImVec2 {x = 40, y = 40} :: IO Imgui.ImVec2)
  let fontSrc = FromTTF "C:\\Windows\\Fonts\\arial.ttf" 22 Nothing Latin
  fonts <- rebuild [fontSrc, Atlas.DefaultFont]
  return $ head fonts

debugGL :: IO ()
debugGL = do
  debugOutput $= Enabled
  debugOutputSynchronous $= Enabled
  debugMessageCallback
    $= Just
      ( \msg ->
          case msg of
            (DebugMessage src DebugTypeError msgId sev str) -> error $ show msg
            _ -> print msg
      )

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

type CanvasSize = Size

type ImguiWinSize = Size

mainLoop :: Window -> Font -> AppState -> GLObjects -> Natural -> Natural -> CanvasSize -> ImguiWinSize -> IO ()
mainLoop
  win
  font
  appState
  glObjects@GLObjects {targetTexture = TextureObject texId, frameBuffer = frameBuf}
  snailPoints
  gridSize
  canvasSz@(Size canvasW canvasH)
  winSize@(Size winW winH) = do
    -- Process the event loop
    GLFW.pollEvents
    close <- GLFW.windowShouldClose win

    unless close do
      -- Tell ImGui we're starting a new frame
      ImguiGL.openGL3NewFrame
      ImguiGLFW.glfwNewFrame
      Imgui.newFrame

      debugInfo 1 "frame initialized"

      GL.clear [ColorBuffer]
      let texPtr = castPtr $ wordPtrToPtr $ WordPtr $ fromIntegral texId
          siz = ImVec2 (fromIntegral canvasW) (fromIntegral canvasH)
          uv0 = ImVec2 0 0
          uv1 = ImVec2 1 1
          tint = ImVec4 1 1 1 1
          bg = tint

      setNextWindowSize (return $ ImVec2 {x = fromIntegral winW, y = fromIntegral winH} :: IO ImVec2) ImGuiCond_Once

      -- Build the GUI
      let body = withWindowOpen "Равлик паскаля" do
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
                when clicked closeCurrentPopup
            newLine

      -- Render
      GL.clear [GL.ColorBuffer]

      let AppState
            { snail = SnailSettings {a = aText, l = lText},
              scaleV = scaleVT,
              translateV = translateVT,
              rotateV = rotateVT,
              rotDeg = rotDegT
            } = appState

      vectors@[scV, transV, rotV] <- mapM vector3StoVec3 [scaleVT, translateVT, rotateVT]

      debugInfo 1 $ "Вектори: " ++ show vectors ++ "\n"
      av <- textToFloatDef 1 <$> readIORef aText
      lv <- textToFloatDef 1 <$> readIORef lText
      rotD <- textToFloatDef 0 <$> readIORef rotDegT
      let transMat = transformMatrix scV transV rotV rotD
      debugInfo 2 $ "Matrix initialized!" ++ show transMat

      -- bindFramebuffer Framebuffer $= frameBuf
      drawPlot snailPoints gridSize glObjects av lv transMat
      -- bindFramebuffer Framebuffer $= defaultFramebufferObject

      debugInfo 3 "Plot ready!"
      withFont font do
        body
      -- showDemoWindow
      -- showAboutWindow
      -- showUserGuide
      -- showMetricsWindow

      render
      ImguiGL.openGL3RenderDrawData =<< getDrawData
      GLFW.swapBuffers win
      mainLoop win font appState glObjects snailPoints gridSize canvasSz winSize