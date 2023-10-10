module Plot where

import Data.Tuple (swap)
import Linear as Lin
import Numeric.Natural (Natural)

data Point a = Point {x :: a, y :: a}

type Plot a = [Point a]

pascalSnail :: (Floating a) => a -> a -> a -> Point a
pascalSnail a l theta =
  let -- r = l - a * sin theta
      xx = (l + a * cos theta) * cos theta
      yy = (l + a * cos theta) * sin theta
   in Point {x = xx, y = yy}

plotPascalSnail :: (Floating a, Enum a) => a -> a -> Natural -> Plot a
plotPascalSnail a l size = map snail [0, step .. (2 * pi)]
  where
    snail = pascalSnail a l
    step = 2.0 * pi / realToFrac size


defaultGridSize :: Natural
defaultGridSize = 10

defaultA :: Floating a => a
defaultA = 1

defaultL :: Floating a => a
defaultL = defaultA

plotGrid :: (Floating a, Enum a) => Natural -> Plot a
plotGrid size =
  let step = 2.0 / realToFrac size
      coords = [-1, step .. 1]
      minCoord = repeat (-1)
      maxCoord = repeat 1
      left = zip minCoord coords
      bottom = map swap left
      right = zip maxCoord coords
      top = map swap right
      horizontal = zipWith zipper left right
      vertical = zipWith zipper bottom top
      zipper (x1, y1) (x2, y2) = [Point {x = x1, y = y1}, Point {x = x2, y = y2}]
   in concat $ horizontal ++ vertical

transformMatrix :: (Floating a) => V3 a -> V3 a -> V3 a -> a -> M44 a
transformMatrix scaleVec translateVec rotateVec rotateDeg =
  scaleM !*! translateThenRotate
  where
    scaleM = scaled $ point scaleVec
    translateThenRotate = mkTransformation (Quaternion rotateDeg rotateVec) translateVec
