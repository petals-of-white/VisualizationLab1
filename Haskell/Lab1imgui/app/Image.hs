module Image where
import Numeric.Natural (Natural)
-- import Graphics.Rendering.OpenGL (PolygonMode(Point))

data Point a = Point {x::a, y::a}

type Plot a = [Point a]

pascalSnail :: Floating a => a -> a -> a -> Point a
pascalSnail a l theta =
    let --r = l - a * sin theta
        xx = (l + a * cos theta) * cos theta
        yy = (l + a * cos theta) * sin theta
    in Point {x=xx, y=yy}

plotPascalSnail :: (Floating a, Enum a) => a -> a -> Natural -> Plot a
plotPascalSnail a l size = map snail [0, step .. (2*pi)]
    where  
        snail = pascalSnail a l
        step = 2.0 *  pi / realToFrac size

-- data Parameters = Parameters 
-- type TMatrix = [[Float]]
-- renderImg :: TMatrix ->
-- renderImg = 
