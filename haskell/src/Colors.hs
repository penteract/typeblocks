module Colors(Col, hslToCol,module Graphics.Gloss.Data.Color) where

import Graphics.Gloss.Data.Color

import Data.Colour.SRGB(toSRGB,RGB(..))
import HSLuv hiding(RGB(..))


type Col = Color --(Double,Double,Double)
{-
black,white::Col
white = (1,1,1)
black = (0,0,0)
-}

hslToCol :: (Double,Double,Double) -> Color
hslToCol (h,s,l) = makeColor (realToFrac r) (realToFrac g) (realToFrac b) 1
    where (RGB r g b) = toSRGB (hsluvToColour (HSLuv (HSLuvHue h) (HSLuvSaturation s) (HSLuvLightness l)))

