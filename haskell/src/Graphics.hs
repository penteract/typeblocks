{-# LANGUAGE NamedFieldPuns #-}
module Graphics where

--import Graphics.Gloss.Data.Picture
-- Pictures, Events
import Graphics.Gloss.Interface.Pure.Game hiding (Display(..),play )
import BoxTypes
import BoxTypesFolds

import Data.List
import Data.Either
import Control.Arrow
import Data.Bifunctor(bimap)
import Data.Function

textDims :: TextBox -> (Float,Float)
textDims (TextBox _ l _) = (l, actualTextHeight)


-- this scaling should really be part of GlossBackend, but that would be slightly ineffecient
nominalTextHeight = 20::Float
actualTextHeight = 119.05 - (-33.33)
unitScale = actualTextHeight / nominalTextHeight

-- Rendering Constants
maxWidth = unitScale * 480 :: Float
spacingH = unitScale * 8 :: Float
paddingH = spacingH
spacingV = spacingH
paddingV = spacingV


type World = [(String, DefnBD)]


layout :: World -> World
layout w = map (second (layoutDefn maxWidth)) w



layoutDefn :: Float -> DefnBD -> DefnBD
layoutDefn maxWidth = undefined
layoutLine :: Float -> LineBD -> LineBD
layoutLine maxWidth (BoxTypes.Line xd l r) = BoxTypes.Line xd{texts=laidTexts,dims=sz} l' r'
  where x = (maxWidth-2*paddingH)
        laidTexts = undefined
        sz = undefined
        (l',r') = undefined

layoutLHS :: Float -> LHSBD -> LHSBD
layoutLHS maxWidth (Operator xd chs) = (Operator xd{texts=laidTexts,dims=sz} laidChs)
  where (laidTexts, laidChs, sz) = layoutThings maxWidth (texts xd) (map (layoutPattern (maxWidth-2*paddingH)) chs)

layoutPattern :: Float -> PatternBD -> PatternBD
layoutPattern maxWidth (Var xd chs) = (Var xd{texts=laidTexts,dims=sz} laidChs)
  where (laidTexts, laidChs, sz) = layoutThings maxWidth (texts xd) (map (layoutLHSAntiHole (maxWidth-2*paddingH)) chs)



-- The creation of these involved some copy and pasting. How should this be accomplished using TypeBlocks?
-- It could probably be done using Generics intelligently, but that's a bit opaque and adds more details for me to worry about
--   Some sort of macros that allow attempting evaluation of untyped terms?
layoutLHSHole :: Float -> LHSHoleBD -> LHSHoleBD
layoutLHSHole maxWidth (LHSHole xd chs) = (LHSHole xd{texts=laidTexts,dims=sz} laidChs)
  where (laidTexts, laidChs, sz) = layoutThings maxWidth (texts xd) (map (layoutLHSAntiHole (maxWidth-2*paddingH)) chs)
layoutLHSAntiHole :: Float -> LHSAntiHoleBD -> LHSAntiHoleBD
layoutLHSAntiHole maxWidth (LHSAntiHole xd chs) = (LHSAntiHole xd{texts=laidTexts,dims=sz} laidChs)
  where (laidTexts, laidChs, sz) = layoutThings maxWidth (texts xd) (map (layoutLHSHole (maxWidth-2*paddingH)) chs)

layoutHole :: Float -> HoleBD -> HoleBD
layoutHole maxWidth (Hole xd chs) = (Hole xd{texts=laidTexts,dims=sz} laidChs)
  where (laidTexts, laidChs, sz) = layoutThings maxWidth (texts xd) (map (layoutExpr (maxWidth-2*paddingH)) chs)
layoutHole maxWidth (Filled xd ch chs) = (Filled xd{texts=laidTexts,dims=sz} laidCh chs)
  where (laidTexts, [laidCh], sz) = layoutThings maxWidth (texts xd) (map (layoutExpr (maxWidth-2*paddingH)) [ch])

layoutExpr :: Float -> ExprBD -> ExprBD
layoutExpr maxWidth (Symbol xd hs) = (Symbol xd{texts=laidTexts,dims=sz} laidHoles)
  where (laidTexts, laidHoles, sz) = layoutThings maxWidth (texts xd) (map (layoutHole (maxWidth-2*paddingH)) hs)

layoutThings :: Ann bx => Float -> [(Int,TextBox)] -> [bx BoxData] -> ([(Int,TextBox)], [bx BoxData],(Float,Float))
layoutThings maxWidth txts bxs = (zip (map fst txts) tbs, bxs'  , sz)
    where
        parts = combine txts bxs
        partDims = map (getDims) (combine txts bxs)

        mrg :: ((Float, Float), (Float, Float)) -> (Float, Float) -> ((Float, Float), (Float, Float))
        mrg ((x,y),(prevWidth, lineheight)) (w,h) =
          if x+prevWidth+w <= maxWidth-paddingH || prevWidth==0
            then ((x+prevWidth, y),(w+spacingH,max lineheight h))
            else ((paddingH, y - lineheight - spacingV), (w,h)) -- new line
        positions = map fst (tail $ scanl' mrg ((paddingH,-paddingV),(0,0)) partDims)
        --lns :: [[((Float,Float),Either TextBox (bx BoxData) )]]
        lns = groupBy ((==) `on` (snd . fst)) (zip positions parts)
        heights :: [Float]
        heights = map (maximum .(0:). map (snd.getDims.snd)) lns
        positionedBoxes =  concat $ zipWith
          (\l h -> map (\((left,top),bx) -> setPos (left,top - (h-snd (getDims bx))/2 ) bx) l )
          lns heights
        sz = foldr (both2 max) (0,0) (map (\bx-> (1,-1)*getPos bx + getDims bx  ) positionedBoxes) + (paddingH,paddingV)
        (tbs,bxs') = partitionEithers positionedBoxes
        --newBoxes = zipWith setPos (map (map snd) lines)



combine :: [(Int,a)] -> [b] -> [Either a b]
combine [] ys = map Right ys
combine ((0,x):xs) ys = Left x : combine xs ys
combine ((n,x):xs) (y:ys) = Right y : combine ((n-1,x):map (first (subtract 1)) xs) ys
combine (txt:_) [] = error "unpaired lists (not enough boxes to match text)"

getDims :: Ann bx => Either TextBox (bx BoxData) -> (Float,Float)
getDims = either textDims (dims . getAnn)
getPos :: Ann bx => Either TextBox (bx BoxData) -> (Float,Float)
getPos = either tPos (position . getAnn)
setPos :: Ann bx => (Float,Float) -> Either TextBox (bx BoxData) -> Either TextBox (bx BoxData)
setPos pos = bimap (\ tb -> tb{tPos=pos}) (modifyAnn (\bd->bd{position=pos}))


both2 :: (a->b->c)->(a,a)->(b,b)->(c,c)
both2 f (x1,y1) (x2,y2)  = (f x1 x2, f y1 y2)
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

draw :: World -> Float -> Picture
draw w t = pictures (map (drawDefn.snd) w)

drawDefn :: DefnBD -> Picture
drawDefn = fst . onDefn (visitVis (\ x ps -> (uncurry translate (position x) (Pictures (drawBox x: ps)), x) ))

drawBox :: BoxData -> Picture
drawBox (BD{texts,dims,borderCol,fillCol,outerShape}) = Pictures (color borderCol (rectLR dims): map (drawText.snd) texts)

rectLR :: (Float,Float) -> Picture
rectLR (w,h) = lineLoop $ [(0,0),(w,0),(w,-h),(0,-h)]

drawText :: TextBox -> Picture
drawText TextBox{tText,tPos} = uncurry translate tPos (text tText)
