{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.IORef
import Data.List (sortBy)
-- import Graphics.GL

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import DearImGui as Imgui
import DearImGui.FontAtlas (addFontFromFileTTF_)
import DearImGui.GLFW as ImguiGLFW
import DearImGui.GLFW.OpenGL as ImguiGLFWGL
import DearImGui.OpenGL3 as ImguiGL
import qualified DearImGui.Raw as ImguiRaw
import DearImGui.Raw.Font (addFontDefault)
import Foreign.C (newCString)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.UI.GLFW (Window)
import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  glfwInitialized <- GLFW.init
  unless glfwInitialized $ error "GLFW init failed"

  runManaged $ do
    mwin <-
      managed $
        bracket
          (GLFW.createWindow 800 600 "Hello, Dear ImGui!" Nothing Nothing)
          (maybe (return ()) GLFW.destroyWindow)
    case mwin of
      Just win -> do
        liftIO $ do
          GLFW.makeContextCurrent (Just win)

          GLFW.swapInterval 1

        -- Create an ImGui context
        _ <- managed $ bracket createContext destroyContext

        styleColorsClassic

        _ <- managed_ $ bracket_ (ImguiGLFWGL.glfwInitForOpenGL win True) ImguiGLFW.glfwShutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ ImguiGL.openGL3Init ImguiGL.openGL3Shutdown
        
        -- let a = unsafeFromByteString
        df <- addFontDefault

        f <- addFontFromFileTTF_ "C:\\Windows\\Fonts\\arial.ttf" 25

        tableRef <-
          liftIO $
            newIORef
              [ (1, "foo"),
                (2, "bar"),
                (3, "baz"),
                (10, "spam"),
                (11, "spam"),
                (12, "spam")
              ]

        liftIO $ mainLoop win tableRef (fromMaybe df f)
      Nothing -> do
        error "GLFW createWindow failed"

  openGL3Shutdown
  GLFW.terminate

mainLoop :: Window -> IORef [(Integer, Text)] -> Font -> IO ()
mainLoop win tableRef font = do
  -- Process the event loop
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close do
    -- Tell ImGui we're starting a new frame

    ImguiGL.openGL3NewFrame
    ImguiGLFW.glfwNewFrame
    Imgui.newFrame

    -- Build the GUI
    name <- newCString "Hello Imgui!!!!"
    let body = bracket_ (ImguiRaw.begin name Nothing Nothing) end do
          -- Add a text widget

          text "Hello, ImGui!"

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

          -- mkTable tableRef
          showDemoWindow

    -- Render
    GL.clear [GL.ColorBuffer]

    withStyleVar
      ImGuiStyleVar_WindowPadding
      (return $ ImVec2 {x = 40, y = 40} :: IO ImVec2)
      (withFont font body)

    render
    ImguiGL.openGL3RenderDrawData =<< getDrawData

    GLFW.swapBuffers win
    mainLoop win tableRef font

mkTable :: IORef [(Integer, Text)] -> IO ()
mkTable tableRef =
  withTableOpen sortable "MyTable" 3 $ do
    tableSetupColumn "Hello"
    tableSetupColumnWith defTableColumnOptions "World"
    withSortableTable \isDirty sortSpecs ->
      when (isDirty && not (null sortSpecs)) do
        -- XXX: do your sorting & cache it. Dont sort every frame.
        putStrLn "So dirty!"
        print sortSpecs
        modifyIORef' tableRef . sortBy $
          foldMap mkCompare sortSpecs

    tableHeadersRow
    readIORef tableRef
      >>= traverse_ \(ix, title) -> do
        tableNextRow
        tableNextColumn $ text (pack $ show ix)
        tableNextColumn $ text title
        tableNextColumn $ void (button "♥")
  where
    mkCompare TableSortingSpecs {..} a b =
      let dir = if tableSortingReverse then flip else id
       in case tableSortingColumn of
            0 -> dir compare (fst a) (fst b)
            1 -> dir compare (snd a) (snd b)
            _ -> EQ

    sortable =
      defTableOptions
        { tableFlags =
            ImGuiTableFlags_Sortable
              .|. ImGuiTableFlags_SortMulti
        }