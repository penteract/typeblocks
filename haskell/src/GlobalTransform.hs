module GlobalTransform where
import Graphics.Gloss.Data.Picture

gTransform :: Picture -> Picture
gTransform pic = scale 0.125 0.125 pic

gInvert :: Point -> Point
gInvert = invertT gTransform

invertT :: (Picture -> Picture)-> Point->Point
invertT f = invert (f (Pictures []))

-- | does not yet handle rotations
invert :: Picture -> Point->Point
invert (Translate dx dy pic ) (x,y) = invert pic (x-dx,y-dy)
invert (Scale sx sy pic) (x,y) = invert pic (x/sx,y/sy)
invert other pos = pos
