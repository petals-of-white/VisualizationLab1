module State where

data SnailSettings = SnailSettings {a :: IORef Text, l :: IORef Text}

data Vector3S = Vector3S (IORef Text) (IORef Text) (IORef Text)

data AppState = AppState {snail :: SnailSettings, scaleV :: Vector3S, translateV :: Vector3S, rotateV :: Vector3S, rotDeg :: IORef Text}

defaultState :: IO AppState
defaultState = do
  aa <- newIORef "1"
  ll <- newIORef "1"
  scaleX <- newIORef "1"
  scaleY <- newIORef "1"
  scaleZ <- newIORef "1"
  trX <- newIORef "0"
  trY <- newIORef "0"
  trZ <- newIORef "0"
  rotX <- newIORef "0"
  rotY <- newIORef "0"
  rotZ <- newIORef "0"
  rotD <- newIORef "0"

  return
    AppState
      { snail = SnailSettings {a = aa, l = ll},
        scaleV = Vector3S scaleX scaleY scaleZ,
        translateV = Vector3S trX trY trZ,
        rotateV = Vector3S rotX rotY rotZ,
        rotDeg = rotD
      }
  where