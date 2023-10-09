{-# LANGUAGE OverloadedStrings #-}

module State where
import Data.IORef
import Data.Text (Text)

data SnailSettings = SnailSettings {a :: IORef Text, l :: IORef Text}

data Vector3S = Vector3S (IORef Text) (IORef Text) (IORef Text)

data AppState = AppState {snail :: SnailSettings, scaleV :: Vector3S, translateV :: Vector3S, rotateV :: Vector3S, rotDeg :: IORef Text}

defaultState :: IO AppState
defaultState = do
  aa <- one
  ll <- one
  scaleX <- one
  scaleY <- one
  scaleZ <- one
  trX <- zero
  trY <- zero
  trZ <- zero
  rotX <-zero
  rotY <-zero
  rotZ <-zero
  rotD <-zero

  return
    AppState
      { snail = SnailSettings {a = aa, l = ll},
        scaleV = Vector3S scaleX scaleY scaleZ,
        translateV = Vector3S trX trY trZ,
        rotateV = Vector3S rotX rotY rotZ,
        rotDeg = rotD
      }
  where
    one = putStrLn "New 'One' allocation" >> newIORef "1"
    zero = putStrLn "New 'One' allocation" >> newIORef "0"