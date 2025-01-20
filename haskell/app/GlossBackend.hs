module GlossBackend where
import Graphics.UI.GLUT.Fonts (stringWidth,fontHeight,StrokeFont(Roman))
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Control.Arrow

import Data.Data

type Animation = Float -> Picture

runUI ::  world                          -- ^ The initial world.
     -> (world -> Animation) -- ^ An action to convert the world to a picture at a particular time.
     -> (Event -> world -> IO world)   -- ^ A function to handle input events.
     -> IO ()
runUI worldStart frameFun handleEvent = playIO (InWindow "typeBlocks" (200, 200) (10, 10)) white 60 -- 60 FPS
   (worldStart,0.0)
   (\ (w,t) -> putStrLn "\n\ndr" >> (return $ frameFun w t))
   (\ e (w,t) -> putStrLn "\n\nev" >> print e >> flip (,) t <$> handleEvent e w )
   (\ dt (w,t) -> putStrLn "\n\ntick" >> return (w,t + dt))


-- Get the length of a piece of text
textLength :: String -> IO Float
textLength s = fromIntegral <$> stringWidth Roman s


-- Find the offset necessary to center text (text needs to be lowered by the value returned here in order to be centered at y = 0)
fontOffset :: IO Float
fontOffset = do
    let top = 119.05
    let bot = -33.33
    f <- Graphics.UI.GLUT.Fonts.fontHeight Roman
    if  abs (f+bot-top) >= 0.001*(top - bot) then do
      print "font Metrics wrong"
      return$ f / 2
    else return$ bot + (f/2)

fontHeight :: IO Float
fontHeight = Graphics.UI.GLUT.Fonts.fontHeight Roman

-- TODO: move to non GLUT specific module
mapChildren :: (Picture -> Picture) -> Picture -> Picture
mapChildren f (Pictures ps)  = Pictures (map f ps)
mapChildren f (Color     a   p) = Color a (f p)
mapChildren f (Translate a b p) = Translate a b (f p)
mapChildren f (Rotate    a   p) = Rotate a (f p)
mapChildren f (Scale     a b p) = Scale a b (f p)
mapChildren f p = p

raiseText :: Float -> Picture -> Picture
raiseText n (Text s) = Translate 0 n (Text s)
raiseText n pic = mapChildren (raiseText n) pic

    --let top = 119.05
    -- let bot = -33.33
    --f <- fontHeight Roman
    -- print (abs (f+bot-top) < 0.01)
