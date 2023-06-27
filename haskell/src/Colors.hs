module Colors where

import Data.Colour.SRGB(toSRGB,RGB(..))
import HSLuv hiding(RGB(..))

type Col = (Double,Double,Double)
black,white::Col
white = (1,1,1)
black = (0,0,0)

hslToCol :: (Double,Double,Double) -> Col
hslToCol (h,s,l) = (r,g,b)
    where (RGB r g b) = toSRGB (hsluvToColour (HSLuv (HSLuvHue h) (HSLuvSaturation s) (HSLuvLightness l)))

