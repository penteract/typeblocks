module Paths where
--import Graphics.Rendering.Cairo
--import Graphics.Rendering.Cairo.Matrix(Matrix(Matrix))
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color(Color)
import Data.Fixed(mod')
import Data.Bits(xor)


--import Utils

instance (Eq a, Eq b, Num a, Num b) => Num (a,b) where
  (a,b) + (w,x) = (a+w,b+x)
  (a,b) * (w,x) = (a*w,b*x)
  negate v = (-1,-1) * v
  abs v = v
  signum (0,0) = 0 -- highest common factor would also satisfy the laws here and could be useful
  signum _ = 1
  fromInteger n = (fromInteger n , fromInteger n) -- Monomorphism restriction! consider enforcing a=b for efficiency

instance (Eq a, Eq b, Fractional a, Fractional b) =>  Fractional (a,b)  where
  fromRational r = (fromRational r,fromRational r)
  (x,y) / (a,b) = (x/a,y/b)
  recip (x,y) = (recip x,recip y)

--type Point = (Float,Float)
--data Path = M Point | L Point | C Point Point Point | A Point Double Double Double | A' Point Double Double Double

-- pi = 3.14159265359

-- Edge description format:
-- describe a path from -4,0 to 4,0 not leaving the box -4,-4 -- 4,4 which could be the top edge for a box
-- path may be scaled
-- Corner description:
-- corners go from 0,4 to 4,0 not leaving the box -4,-4 -- 4,4

--type Path = [Point]
type Corner = Path
type Edge = Path

data BoxShape = BoxShape Corner Edge Corner Edge Corner Edge Corner Edge
instance Show BoxShape
  where show x = "<>"
symmetric :: Corner -> Edge -> BoxShape
symmetric c e = BoxShape c e c e c e c e

straight :: Edge
straight = [(-4, 0),(4, 0)]
sqEdge :: Edge
sqEdge = [(-4, 0), (-2, 0), (-2, -4), (2, -4), (2, 0),(4, 0)]
hat :: Edge
hat = [(-4, 0), (0, -4), (4, 0)]
spike :: Edge
spike = [(-4, 0), (-2, 0), (0, -4), (2, 0), (4, 0)]

zigzag :: Edge
zigzag = [(-4, 0), (0, -4), (0, 4), (4, 0)]
--https://math.stackexchange.com/a/1671684
-- lump = [(-4, 0), curveTo (-4) (-16/3) 4 (-16/3) 4 0]
--

-- take n samples, evenly spaced from 0 to 1 (including each endpoint
sample :: Integer -> (Float -> Point) -> Path
sample n f = [f (fromInteger i/fromInteger n)  | i <- [0..n] ]
circ :: Float -> Float -> Point
circ r t = (-r*cos (t*pi), r*sin (t*pi))

lump :: Edge
lump = sample 10 (circ 4)   --[(-4, 0), curveTo (-4) (-16/3) 4 (-16/3) 4 0]


j2 :: Edge
j2 = [(-4, 3), (-2, 4), (-2, -4), (2, -4), (2, 4),(4, 3)]

{-
intshape :: Edge
intshape = (-4, 0): pts ++ [(4, 0)]
  where
      n = 3
      pts = map (\ x -> (\k -> (k, ()(((x-1)`mod'`2)*2-1)*(abs k-4))) (4*x/n) ) [-n..n]
      -}
{-square :: Corner
square = [(0, -4), (4, 0)] -}
square :: Corner
square = [(0, -4), (0, 0), (4, 0)]

simple :: Edge -> BoxShape
simple = symmetric square

rect :: BoxShape
rect = simple straight
r2 = BoxShape square j2 square spike square zigzag square lump


translatePath :: Point -> Path -> Path
translatePath (dx,dy) = map (\(x,y)->(x+dx, y+dy))

rotatePath :: Integer -> Path -> Path -- rotate clockwise by a multiple of 90 degrees
rotatePath 0 = id
rotatePath 1 = map (\(x,y)->(y, -x))
rotatePath 2 = map (\(x,y)->(-x, -y))
rotatePath 3 = map (\(x,y)->(-y, x))

-- flip horizontally; vertically.
flipH = reverse . map (\(x,y) -> (-x,y) )
flipV = reverse . map (\(x,y) -> (x,-y) )

drawBox :: (Color,Color) -> (Float,Float) -> BoxShape -> Picture
drawBox cols (w,h) sh = let pth =  mkPath (max 16 w, max 16 h) sh
                          in
                          (if min w h < 16 then scale (min 1 (w/16)) (min 1 (w/16)) else id)
                           (Pictures [color (fst cols) (polygon pth) ,color (snd cols) (lineLoop pth)])

mkPath :: (Float,Float) -> BoxShape -> Path
mkPath (w,h) (BoxShape tl top tr right br bottom bl left) =
  let mx = w/2
      my = h/2
  in
     concat$ zipWith ($) [
      id,
        translatePath (mx,0),
          translatePath (w,0).flipH,
            translatePath (w,-my) . rotatePath 1,
              translatePath (w,-h) . rotatePath 2,
                translatePath (mx,-h) . rotatePath 2,
                  translatePath (0,-h) . flipV,
                    translatePath (0,-my) . rotatePath 3
      ] [tl,top,tr,right,br,bottom,bl,left]


shapes = map simple [sqEdge,hat,spike,zigzag,lump]

-- | checks if a point is inside the first polygon of a picture
--   Assumes that the first thing is actually a polygon
inFirstPicture :: (Float,Float) -> Picture-> Bool
inFirstPicture pos (Color _ pic) = inFirstPicture pos pic
inFirstPicture (x,y) (Translate dx dy pic ) = inFirstPicture (x-dx,y-dy) pic
inFirstPicture (x,y) (Scale sx sy pic) = inFirstPicture (x/sx,y/sy) pic
inFirstPicture pos (Pictures (h:t)) = inFirstPicture pos h
inFirstPicture pos (Polygon path) = pos `inside` path

{-
inShape :: (Float,Float) -> (Float,Float) -> BoxShape -> Bool
inShape pos sz sh = inRect (fst pos,-snd pos) (-4) (sz+4) && case drawBox undefined sz sh of
                                                 Scale sx sy (Pictures (Color _ (Polygon pth):_)) -> (pos / (sx,sy)) `inside` pth
                                                 Pictures (Color _ (Polygon pth) : _) -> up pos `inside` up pth
                                                 -}
inRect (x,y) (x1,y1) (x2,y2) = x>=x1 && y>=y1 && x<=x2 && y<=y2
-- | Determine if a point is inside a polygon
--     even-odd fill rule, if clockwise and point is on boundary, count it.
--       In the edge case of overlapping horizontal edges, this will not count a point on both edges.
inside :: (Float,Float) -> [(Float,Float)] -> Bool
inside pt (x:xs) = foldl' xor False $ zipWith (leftOf pt) (x:xs) (xs++[x])

-- determine if a point is to the left of a line segment in 2D (half open, includes start, not end)
-- if point is on the line, count it if the line is going down, don't count it if the line is going up
--   if line is horizontal, count it if and only if the point is on the line segment
leftOf :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
leftOf (x,y) (x1,y1) (x2,y2) = case compare y1 y2 of
  -- going up
  LT -> if y<y1 || y>=y2 then False else (x-x1)*(y2-y1) < (x2-x1)*(y-y1)
  -- going down
  GT -> if y<=y2 || y>y1 then False else (x-x1)*(y2-y1) >= (x2-x1)*(y-y1)
  -- horizontal
  EQ -> if y/=y1 then False else if x1<x2 then x>=x1 && x<x2 else x>x2 && x<=x1

