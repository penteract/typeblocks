module Graphics where

--import Graphics.Gloss.Data.Picture
-- Pictures, Events
import Graphics.Gloss.Interface.Pure.Game hiding (Display(..),play )
import BoxTypes

type World = [(String, DefnBD)]

addSizes :: World -> World
addSizes w = undefined

draw :: World -> Float -> Picture
draw w t = undefined
