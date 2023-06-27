module Paths where
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Matrix(Matrix(Matrix))
import Data.Fixed(mod')

type Pt = (Double,Double)
--data Path = M Pt | L Pt | C Pt Pt Pt | A Pt Double Double Double | A' Pt Double Double Double

pi = 3.14159265359

-- Edge description format:
-- describe a path from -4,0 to 4,0 not leaving the box -4,-4 -- 4,4 which could be the top edge for a box
-- path may be scaled
-- Corner description:
-- corners go from 0,4 to 4,0 not leaving the box -4,-4 -- 4,4


type Corner = Path
type Edge = Path

data BoxShape = BoxShape Corner Edge Corner Edge Corner Edge Corner Edge
instance Show BoxShape
  where show x = "<>"
symmetric :: Corner -> Edge -> BoxShape
symmetric c e = BoxShape c e c e c e c e

straight :: Edge
straight = [LineTo (-4) 0,LineTo 4 0]
sqEdge :: Edge
sqEdge = [LineTo (-4) 0, LineTo (-2) 0, LineTo (-2) (-4), LineTo 2 (-4), LineTo 2 0,LineTo 4 0]
hat :: Edge
hat = [LineTo (-4) 0, LineTo 0 (-4), LineTo 4 0]
spike :: Edge
spike = [LineTo (-4) 0, LineTo (-2) 0, LineTo 0 (-4), LineTo 2 0, LineTo 4 0]

zigzag = [LineTo (-4) 0, LineTo 0 (-4), LineTo 0 4, LineTo 4 0]
--https://math.stackexchange.com/a/1671684
lump = [LineTo (-4) 0, CurveTo (-4) (-16/3) 4 (-16/3) 4 0]

j2 :: Edge
j2 = [LineTo (-4) 3, LineTo (-2) (4), LineTo (-2) (-4), LineTo 2 (-4), LineTo 2 4,LineTo 4 3]


intshape :: Edge
intshape = LineTo (-4) 0: pts ++ [LineTo 4 0]
  where
      n = 3
      pts = map (\ x -> (\k -> LineTo k ((((x-1)`mod'`2)*2-1)*(abs k-4))) (4*x/n) ) [-n..n]

square :: Corner
square = [LineTo 0 4, LineTo 0 0, LineTo 4 0]

simple :: Edge -> BoxShape
simple = symmetric square

rect :: BoxShape
rect = simple straight
r2 = BoxShape square j2 square spike square zigzag square lump

drawBox :: Pt -> BoxShape -> Render ()
drawBox (w,h) (BoxShape tl top tr right br bottom bl left) = do
    newPath
    appendPath tl
    translate (w/2) 0
    appendPath top
    transform (Matrix 0 1 (-1) 0 (w/2) 0)
    appendPath tr
    translate (h/2) 0
    appendPath right
    transform (Matrix 0 1 (-1) 0 (h/2) 0)
    appendPath br
    translate (w/2) 0
    appendPath bottom
    transform (Matrix 0 1 (-1) 0 (w/2) 0)
    appendPath bl
    translate (h/2) 0
    appendPath left
    transform (Matrix 0 1 (-1) 0 (h/2) 0)-- return to original CTM
    closePath

shapes = map simple [sqEdge,hat,spike,zigzag,lump]
