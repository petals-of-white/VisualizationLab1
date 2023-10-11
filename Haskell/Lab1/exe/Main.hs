module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core as Core
main :: IO ()
main = do
  startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do

  _ <- return window # set UI.title "Pascal Snail"
  original <- UI.image # set UI.width 100 # set UI.height 100 # set UI.alt "Wow"
  transformed <- UI.image # set UI.width 100 # set UI.height 100 # set UI.alt "Kek"
  button <- UI.button # set UI.text "Перетворити!"
  _ <- getBody window #+ [column [element original, element transformed, element button]]
  on UI.click button $ const $ do
        element button # set UI.text "I have been clicked!"
-- Gtk.init Nothing

  -- win <- new Gtk.Window [ #title := "Hi there" ]

  -- on win #destroy Gtk.mainQuit

  -- button <- new Gtk.Button [ #label := "Click me" ]

  -- on button #clicked (set button [ #sensitive := False,
  --                                  #label := "Thanks for clicking me" ])

  -- #add win button

  -- #showAll win

  -- Gtk.main