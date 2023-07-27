module Graphics where

--import Linear (V4(..))
import Graphics.Rendering.Cairo
import BoxTree
import Data.List
import Data.Tree hiding (drawTree)
import Control.Arrow
import System.IO.Unsafe
import Data.Function

sup a b = seq (unsafePerformIO (print a)) b


-- Rendering Constants
maxWidth = 480 :: Double
spacingH = 8 :: Double
paddingH = spacingH
spacingV = spacingH
paddingV = spacingV --spacingV

txtheight = 20::Double
data FontInfo = FI {
  height::Double
}

renderTree :: FontInfo -> BoxTree -> Render ()
renderTree fi = (\ t -> renderRTree t >> translate 0 (snd (size t)) ) . drawTree fi maxWidth

-- | box drawing instructions, subboxes with offsets, width and height
data RTree = RTree {
    rendering :: Render()
  , subboxes :: [((Double,Double),RTree)]
  , size :: (Double,Double)
}

combine :: [(Int,a)] -> [b] -> [Either a b]
combine [] ys = map Right ys
combine ((0,x):xs) ys = Left x : combine xs ys
combine ((n,x):xs) (y:ys) = Right y : combine ((n-1,x):xs) ys
combine (txt:_) [] = error "unpaired lists (not enough boxes to match text)"

drawText :: FontInfo -> TextBox -> RTree
drawText (FI h) (TextBox txt len) = RTree (moveTo 0 0 >> showText txt) [] (len,h)

both2 :: (a->b->c)->(a,a)->(b,b)->(c,c)
both2 f (x1,y1) (x2,y2)  = (f x1 x2, f y1 y2)

(-.-) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(-.-) = uncurry (***) . ((-) *** (-)) --(x1-x2, y1-y2)

(+.+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+.+) = uncurry (***) . ((+) *** (+))

drawTree :: FontInfo -> Double -> BoxTree -> RTree
drawTree fi maxWidth (Node root children) = RTree
      (do
        --liftIO (print (200+length children))
        rectangle 0 0 w h
        setSourceCol (fillCol root)
        fillPreserve
        setSourceCol (borderCol root)
        stroke
        )
      subboxes
      (w,h)
    where
        parts = map (either (drawText fi) (drawTree fi (maxWidth-2*paddingH))) (combine (texts root) (children))
        positions = map fst (tail $ scanl'  mrg ((paddingH,paddingV),(0,0)) parts)
        mrg :: ((Double, Double), (Double, Double)) -> RTree -> ((Double, Double), (Double, Double))
        mrg ((x,y),(prevWidth, lineheight)) (RTree _ _ (w,h)) =
          if x+prevWidth+w <= maxWidth-paddingH || prevWidth==0
            then ((x+prevWidth, y),(w+spacingH,max lineheight h))
            else ((paddingH,y+lineheight+spacingV) ,(w,h)) -- new line
        height = snd.size
        lns :: [[((Double,Double),RTree)]]
        lns = groupBy ((==) `on` (snd . fst)) (zip positions parts)
        hs :: [Double]
        hs = map (maximum .(0:). map (height.snd)) lns
        subboxes = concat $ zipWith (\ h -> map (\ ((x,y),r) -> ((x,y+(h-height r)/2),r))) hs lns
        --hts = map (map $ uncurry (flip  ((+.+) . size))) lns
        -- subboxes = zip positions parts
        (w,h) = foldr (both2 max) (0,0) (map (\(pos,rt)->pos+.+size rt) subboxes) +.+ (paddingH,paddingV)

renderRTree :: RTree -> Render ()
renderRTree (RTree rendering subboxes _) = do
  rendering
  mapM_ (\((x,y),sb)->do
    save
    translate x y
    renderRTree sb
    restore
    ) subboxes
