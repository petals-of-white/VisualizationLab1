module Plot where

import Linear as Lin
import Numeric.Natural (Natural)

data Point a = Point {x :: a, y :: a} deriving (Show)

data SnailOptions t = SnailOptions {a :: t, l :: t} deriving Eq

instance Functor SnailOptions where
  fmap f SnailOptions {a = aa, l = ll} = SnailOptions {a = f aa, l = f ll}

data Options t = Options
  { snailOptions :: SnailOptions t,
    scaleVector :: V3 t,
    translateVector :: V3 t,
    rotation :: Quaternion t
  } deriving Eq

instance Functor Options where
  fmap f (Options snail scaleV translateV rot) =
    Options (fmap f snail) (fmap f scaleV) (fmap f translateV) (fmap f rot)

defaultSnailOptions :: (Floating a) => SnailOptions a
defaultSnailOptions = SnailOptions {a = 2, l = 1}

defaultScaleV :: (Floating a) => V3 a
defaultScaleV = V3 1 1 1

defaultRotation :: (Floating a) => Quaternion a
defaultRotation = Quaternion 0 (V3 0 0 0)

defaultTranslateV :: (Floating a) => V3 a
defaultTranslateV = V3 0 0 0

defaultOptions :: (Floating a) => Options a
defaultOptions = Options defaultSnailOptions defaultScaleV defaultTranslateV defaultRotation

type Plot a = [Point a]

pascalSnail :: (Floating a) => a -> a -> a -> Point a
pascalSnail a l theta =
  let -- r = l - a * sin theta
      xx = (l + a * cos theta) * cos theta
      yy = (l + a * cos theta) * sin theta
   in Point {x = xx, y = yy}

plotPascalSnail :: (Floating a, Enum a) => a -> a -> Natural -> Plot a
plotPascalSnail a l size = map snail [0, 0 + step .. (2 * pi)]
  where
    snail = pascalSnail a l
    step = 2.0 * pi / realToFrac size

plotGrid :: (Floating a, Enum a) => Natural -> Plot a
plotGrid size =
  let step = 2.0 / realToFrac size
      coords = [-1.0, -1.0 + step .. 1.0]
      left = zip minCoords coords
      bottom = zip coords minCoords
      right = zip maxCoords coords
      top = zip coords maxCoords
      horizontal = concat $ zipWith zipper left right
      vertical = concat $ zipWith zipper bottom top
   in vertical ++ horizontal
  where
    minCoords = repeat (-1)
    maxCoords = repeat 1
    zipper (x1, y1) (x2, y2) = [Point {x = x1, y = y1}, Point {x = x2, y = y2}]

toRadians :: (Floating a) => a -> a
toRadians = (*) (pi / 180)

transformMatrix :: (Floating a) => V3 a -> V3 a -> Quaternion a -> M44 a
transformMatrix scaleVec translateVec rotateQuaternion =
  scaleM !*! translateRotate
  where
    scaleM = scaled $ point scaleVec
    translateRotate = mkTransformation rotateQuaternion translateVec
