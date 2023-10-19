{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module GUI where

import Control.Exception (bracket)
import Control.Monad (unless, when, zipWithM_)
import Control.Monad.Managed hiding (with)
import Data.Foldable (Foldable (toList))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import DearImGui as Imgui hiding (ImVec3 (..), ImVec4 (x, y))
import DearImGui.FontAtlas as Atlas
import DearImGui.GLFW as ImguiGLFW (glfwNewFrame)
import qualified DearImGui.OpenGL3 as ImguiGL
import Foreign (WordPtr (WordPtr), castPtr, wordPtrToPtr)
import Foreign.Marshal (with)
import GHC.Natural (Natural)
import GLHelpers
import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Linear (Quaternion (Quaternion), V3 (..))
import Plot
import Text.Read (readMaybe)

type CanvasSize = Size

type ImguiWinSize = Size

type V3RefText = V3 (IORef Text)

type QuaternionRefText = Quaternion (IORef Text)

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
  let fontSrc = FromTTF "C:\\Windows\\Fonts\\arial.ttf" 26 Nothing Latin
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

valueInput :: Text -> IORef Text -> IO Bool
valueInput label ref = withItemWidth 200 $ inputText label ref 6

defaultState :: IO (Options (IORef Text))
defaultState = do
  let Options
        { snailOptions = SnailOptions aa ll,
          scaleVector = scaleVec,
          translateVector = translateVec,
          rotation = rotat
        } = fmap (pack . show) defaultOptions

  scaleV <- mapM newIORef scaleVec
  translateV <- mapM newIORef translateVec
  rot <- mapM newIORef rotat
  [aRef, lRef] <- mapM newIORef [aa, ll]
  pure
    Options
      { snailOptions = SnailOptions {a = aRef, l = lRef},
        scaleVector = scaleV,
        translateVector = translateV,
        rotation = rot
      }

mainLoop :: Window -> Font -> Options (IORef Text) -> GLObjects -> Natural -> Natural -> CanvasSize -> ImguiWinSize -> IO ()
mainLoop
  win
  font
  appState@Options
    { snailOptions = SnailOptions {a = aa, l = ll},
      rotation = rotateQT@(Quaternion rotDT (V3 rx ry rz)),
      translateVector = translateVT@(V3 tx ty tz),
      scaleVector = scaleVT@(V3 sx sy sz)
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
            withGroup $ do
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
              return ()

            sameLine
            with siz \szPtr ->
              with uv0 \uv0Ptr ->
                with uv1 \uv1Ptr ->
                  with tint \tintPtr ->
                    with bg \bgPtr ->
                      image texPtr szPtr uv0Ptr uv1Ptr tintPtr bgPtr

            -- Add a button widget, and call 'putStrLn' when it's clicked
            reset <- button "Reset"
            when reset $ setDefaults appState
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

      snailT <- mapM readIORef [aa, ll]
      let sus = maybe defaultSnailOptions (\[aS, lS] -> SnailOptions aS lS) (mapM ((readMaybe :: String -> Maybe Float) <$> unpack) snailT)

      -- av <- textToFloatDef 1 <$> readIORef aa
      -- lv <- textToFloatDef 1 <$> readIORef ll
      let transMat = transformMatrix (fromMaybe defaultScaleV scV) (fromMaybe defaultTranslateV transV) (Quaternion (toRadians rotDeg) trVec)
      debugInfo 2 $ "Matrix initialized!" ++ show transMat

      oldViewPort <- get viewport

      bindFramebuffer Framebuffer $= frameBuf
      viewport $= (Position 0 0, canvasSz)

      drawPlot snailPoints gridSize glObjects sus transMat
      bindFramebuffer Framebuffer $= defaultFramebufferObject
      viewport $= oldViewPort

      debugInfo 3 "Plot ready!"

      withFont font body

      render
      debugInfo 1 "NextFrame rendered"

      ImguiGL.openGL3RenderDrawData =<< getDrawData
      debugInfo 2 "new frame rendered!!!"

      GLFW.swapBuffers win

      mainLoop win font appState glObjects snailPoints gridSize canvasSz winSize
    debugInfo 1 "Closing GLFW window..."

    where Options { snailOptions = SnailOptions adef ldef,
                    scaleVector = scaleDef,
                    translateVector = translateDef,
                    rotation = rotDef
          } = defaultOptions

setDefaults :: Options (IORef Text) -> IO ()
setDefaults
  Options
    { snailOptions = SnailOptions aa ll,
      scaleVector = scaleVec,
      translateVector = transVec,
      rotation = rot
    } = do
    let Options
          { snailOptions = SnailOptions adef ldef,
            scaleVector = scaleDef,
            translateVector = translateDef,
            rotation = rotDef
          } = fmap (pack . show) defaultOptions

    zipWithM_ writeIORef [aa, ll] [adef, ldef]
    zipWithM_ writeIORef (toList scaleVec) (toList scaleDef)
    zipWithM_ writeIORef (toList transVec) (toList translateDef)
    zipWithM_ writeIORef (toList rot) (toList rotDef)