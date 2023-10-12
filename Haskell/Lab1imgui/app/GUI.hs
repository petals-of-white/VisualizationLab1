{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Control.Exception (bracket)
import Control.Monad (replicateM, unless, when)
import Control.Monad.Managed hiding (with)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
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
import Linear (Quaternion (Quaternion), V3 (..))
import Plot (defaultRotation, defaultScaleV, defaultSnailOptions, defaultTranslateV, transformMatrix, toRadians)
import Text.Read (readMaybe)

makeWindow :: Size -> Managed (Maybe Window)
makeWindow (Size width height) =
  managed $
    bracket
      (GLFW.createWindow (fromIntegral width) (fromIntegral height) "Pascal Snail" Nothing Nothing)
      (maybe (return ()) GLFW.destroyWindow)

initGLFW :: IO ()
initGLFW = do
  setErrorCallback (Just \err msg -> putStrLn $ show err ++ " " ++ msg)
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "GLFW init failed"

addGlobalStyles :: IO Font
addGlobalStyles = do
  styleColorsClassic
  pushStyleVar ImGuiStyleVar_WindowPadding (return $ Imgui.ImVec2 {Imgui.x = 40, Imgui.y = 40} :: IO Imgui.ImVec2)
  let fontSrc = FromTTF "C:\\Windows\\Fonts\\arial.ttf" 22 Nothing Latin
  fonts <- rebuild [fontSrc, Atlas.DefaultFont]
  return $ head fonts

debugGL :: IO ()
debugGL = do
  debugOutput $= Enabled
  debugOutputSynchronous $= Enabled
  debugMessageCallback
    $= Just print
    >> putStrLn ""

textToFloat :: Text -> Maybe Float
textToFloat = readMaybe . unpack

textToFloatDef :: Float -> Text -> Float
textToFloatDef def = fromMaybe def . textToFloat

-- textRefToFloat :: IORef Text -> IO
data SnailSettings = SnailSettings {a :: IORef Text, l :: IORef Text} deriving (Eq)

type V3RefText = V3 (IORef Text)

type QuaternionRefText = Quaternion (IORef Text)

v3RefTextToVec3 :: V3RefText -> IO (Maybe (V3 Float))
v3RefTextToVec3 (V3 xref yref zref) = do
  texts <- mapM readIORef [xref, yref, zref]
  let values = mapM textToFloat texts
  return $ do
    [xx, yy, zz] <- values
    return $ V3 xx yy zz

quatRefTextToquat :: QuaternionRefText -> IO (Maybe (Quaternion Float))
quatRefTextToquat (Quaternion deg (V3 xref yref zref)) = do
  texts <- mapM readIORef [xref, yref, zref, deg]
  let values = mapM textToFloat texts
  return $ do
    [xx, yy, zz, degg] <- values
    return $ Quaternion degg (V3 xx yy zz)

-- return $ Quaternion deg (V3 (textToFloatDef 1 xv) (textToFloatDef 1 yv) (textToFloatDef 1 zv))

data AppState = AppState
  { snail :: SnailSettings,
    scaleV :: V3RefText,
    translateV :: V3RefText,
    rotateQ :: QuaternionRefText
  }
  deriving (Eq)

valueInput :: (MonadIO m) => Text -> IORef Text -> m Bool
valueInput label ref = inputText label ref 6

defaultState :: IO AppState
defaultState = do
  let a = defaultSnailOptions
  [aa, ll, scaleX, scaleY, scaleZ] <- replicateM 5 $ newIORef "1"
  [trX, trY, trZ, rotX, rotY, rotZ, rotD] <- replicateM 7 $ newIORef "0"
  pure
    AppState
      { snail = SnailSettings {GUI.a = aa, GUI.l = ll},
        scaleV = V3 scaleX scaleY scaleZ,
        translateV = V3 trX trY trZ,
        rotateQ = Quaternion rotD (V3 rotX rotY rotZ)
      }

type CanvasSize = Size

type ImguiWinSize = Size

mainLoop :: Window -> Font -> AppState -> GLObjects -> Natural -> Natural -> CanvasSize -> ImguiWinSize -> IO ()
mainLoop
  win
  font
  appState@AppState
    { snail = SnailSettings {GUI.a = aa, GUI.l = ll},
      rotateQ = rotateQT@(Quaternion rotDT rotVT@(V3 rx ry rz)),
      translateV = translateVT@(V3 tx ty tz),
      scaleV = scaleVT@(V3 sx sy sz)
    }
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

      setNextWindowSize (return $ ImVec2 {Imgui.x = fromIntegral winW, Imgui.y = fromIntegral winH} :: IO ImVec2) ImGuiCond_Once

      -- Build the GUI
      let body = withWindowOpen "Pascal Snail" do
            -- Add a text widget
            text "Pascal Snail"
            _ <- valueInput "a" aa
            _ <- valueInput "l" ll

            text "scale vector"
            _ <- valueInput "sx" sx
            _ <- valueInput "sy" sy
            _ <- valueInput "sz" sz

            text "translation vector"
            _ <- valueInput "tx" tx
            _ <- valueInput "ty" ty
            _ <- valueInput "tz" tz

            text "rotation vector"
            _ <- valueInput "rx" rx
            _ <- valueInput "ry" ry
            _ <- valueInput "rz" rz
            _ <- valueInput "Deg" rotDT

            with siz \szPtr ->
              with uv0 \uv0Ptr ->
                with uv1 \uv1Ptr ->
                  with tint \tintPtr ->
                    with bg \bgPtr ->
                      image texPtr szPtr uv0Ptr uv1Ptr tintPtr bgPtr

            -- Add a button widget, and call 'putStrLn' when it's clicked
            reset <- button "Reset"
            -- when reset $ setDefaults appState
            itemContextPopup do
              text "pop!"
              button "ok" >>= \clicked ->
                when clicked closeCurrentPopup
            newLine

      -- Render
      GL.clear [GL.ColorBuffer]

      v3s@[scV, transV] <- mapM v3RefTextToVec3 [scaleVT, translateVT]

      Quaternion rotDeg trVec <- fromMaybe defaultRotation <$> quatRefTextToquat rotateQT
      debugInfo 1 $ "Vectors: " ++ show v3s ++ "\n"
      av <- textToFloatDef 1 <$> readIORef aa
      lv <- textToFloatDef 1 <$> readIORef ll
      let transMat = transformMatrix (fromMaybe defaultScaleV scV) (fromMaybe defaultTranslateV transV) (Quaternion (toRadians rotDeg) trVec)
      debugInfo 2 $ "Matrix initialized!" ++ show transMat

      oldViewPort <- get viewport
      debugInfo 10 $ "Old view port: " ++ show oldViewPort

      bindFramebuffer Framebuffer $= frameBuf
      viewport $= (Position 0 0, canvasSz)
      debugInfo 3 $ "New view port " ++ show canvasSz

      drawPlot snailPoints gridSize glObjects av lv transMat
      bindFramebuffer Framebuffer $= defaultFramebufferObject
      viewport $= oldViewPort

      debugInfo 3 "Plot ready!"

      withFont font do
        body
        -- showDemoWindow
        -- showAboutWindow
        -- showUserGuide
        showMetricsWindow

      render
      debugInfo 1 "NextFrame rendered"

      ImguiGL.openGL3RenderDrawData =<< getDrawData
      debugInfo 2 "new frame rendered!!!"

      GLFW.swapBuffers win

      mainLoop win font appState glObjects snailPoints gridSize canvasSz winSize
    debugInfo 1 "Closing GLFW window..."

-- setDefaults :: AppState -> IO ()
-- setDefaults AppState
--   {snail=SnailSettings {GUI.a=a,GUI.l=l}, scaleV=scaleV, translateV=translateV, rotate} =
