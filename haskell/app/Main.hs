{-#LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Control.Exception
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Managed
import Linear (V4(..))
import qualified Data.Map as Map
-- import DearImGui
-- import DearImGui.OpenGL2
-- import DearImGui.SDL
-- import DearImGui.SDL.OpenGL
-- import Graphics.GL
import SDL
import SDL.Cairo
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix(Matrix(..))
import Graphics
import BoxTree
import Paths
import Parsing

import Data.Tree
import Language.Haskell.Exts

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll
  ParseOk mod <- parseFile "test/Sample.hs"
  (_,bxs )<- moduleToBoxes mod Map.empty
  print (length bxs)
  runManaged (do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Typeblocks"
      let config =  WindowConfig {
          windowBorder          = True
        , windowHighDPI         = False
        , windowInputGrabbed    = False
        , windowMode            = Windowed
        , windowGraphicsContext = NoGraphicsContext
        , windowPosition        = Wherever
        , windowResizable       = True
        , windowInitialSize     = V2 800 600
        , windowVisible         = True
        }
  --defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL, windowResizable = True }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    -- glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    -- _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    -- _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    -- _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    tx <- liftIO (createCairoTexture' renderer window) -- (V2 2 600)
    liftIO $ withCairoTexture tx $ do
      --
      FontExtents asc desc h _ _ <- fontExtents

      setFontMatrix (Matrix 10 0 0 20 0 asc)
    liftIO $ mainLoop renderer 0 window bxs tx
    )


--mainLoop :: Renderer -> Int -> IO ()
-- mainLoop r s = do
--   ev <- waitEvent
--   if case eventPayload ev of
--         KeyboardEvent k -> keyboardEventKeyMotion k == Pressed && keysymKeycode (keyboardEventKeysym k) == KeycodeQ
--         _ -> False
--        then return ()
--        else do
--             rendererDrawColor r $= V4 0 0 s 128
--             clear r
--             present r
--             mainLoop r (s)

e1 :: Render BoxTree
e1 = (\t -> Node (defaultData{texts=[(0,t)]}) []) <$> makeText "helloMy"
e2 = (\e1 -> Node (defaultData) [e1,e1,e1]) <$> e1
e3 = liftA2 (\e2 t -> Node (defaultData{texts=[(0,t),(2,t)]}) [e2,e2,e2]) e1  (makeText "+")

mainLoop r s w bxs tx = do
  withCairoTexture tx $ do
    setSourceRGBA 0.5 0.5 0.5 1
    paint
    setSourceRGBA 1 1 1 1
    translate 80 100
    drawBox (100,300) r2
    stroke
    translate 60 60
    getFontMatrix >>= liftIO . print
    showText ('h':"ello")
    getFontMatrix >>= liftIO . print
    showText ('h':"ellol")
    getFontMatrix >>= liftIO . print
    translate (-40) (-40)
    setSourceRGBA 1 0 0 1
    moveTo 100 (100+s)
    lineTo 200 200.1
    lineTo 100 200
    closePath
    setSourceRGBA 0 1 0 1
    strokePreserve
    setSourceRGBA 1 0 0 1
    fill
    setFontMatrix (Matrix 15 0 0 15 0 0)
    ex <- textExtents ['x']
    let xheight = negate (textExtentsYbearing ex) -- I should do this properly when I stop using the "toy" library
    f@(FontExtents asc desc h _ _) <- fontExtents
    let unpadded = (h-spacingV) -- h-paddingV*2 is also reasonable
    --liftIO (print f)
    liftIO (putStrLn$ Data.Tree.drawTree  (fmap show (snd$head bxs)))
    --let n = asc+desc
    setFontMatrix (Matrix 15 0 0 15 0 ((unpadded+xheight)/2))
    --e3 >>= renderTree (FI unpadded)
    mapM_ (renderTree (FI unpadded).snd) bxs
    --moveTo 400 400
    --showText ('h':"ellolasdfdfasdfasdfasdf")
    --lineTo 400 600

    --showText ('h':"e;llolasdfdfasdfasdfasdf")
    --lineTo 500 700
    --closePath

    --stroke
  rendererDrawColor r $= V4 0 0 0 255
  copy r tx Nothing Nothing
  present r
  clear r
  ev <- waitEvent
  --putStrLn (show ev)
  case eventPayload ev of
        KeyboardEvent k -> if keyboardEventKeyMotion k == Pressed && keysymKeycode (keyboardEventKeysym k) == KeycodeQ then return () else mainLoop r (s+1) w bxs tx
        WindowSizeChangedEvent (WindowSizeChangedEventData{windowSizeChangedEventSize = v}) -> createCairoTexture' r w >>= mainLoop r s w bxs
        _ -> mainLoop r s w bxs tx
