module Image where

pascalSnail :: Floating a => a -> a -> a -> (a, a)
pascalSnail theta a l =
    let r = l - a * sin theta
        x = (l + a * cos theta) * cos theta
        y = (l + a * cos theta) * sin theta
    in (x,y)

plotPascalSnail

-- data Parameters = Parameters 
-- type TMatrix = [[Float]]
-- renderImg :: TMatrix ->
-- renderImg = 
