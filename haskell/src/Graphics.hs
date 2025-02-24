{-# LANGUAGE NamedFieldPuns #-}
module Graphics where

--import Graphics.Gloss.Data.Picture
-- Pictures, Events
import Graphics.Gloss.Interface.Pure.Game hiding (Display(..),play )
import Types
import Colors
import Folds
import Paths(drawBox)

import Data.List
import Data.Either
import Control.Arrow
import Data.Bifunctor(bimap)
import Data.Function
import GlobalTransform
import Data.Foldable(toList)

--import Utils(sup,up)

textDims :: TextBox -> (Float,Float)
textDims (TextBox _ l _) = (l, actualTextHeight - 2*spacingV)


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


type World = (Maybe (Pickable,(Float,Float)), [(String, DefnBD)])
-- (Maybe Pickable,[(String, DefnBD)])


layout :: World -> World
layout (dragging, w) = let
               w' = map (second (layoutDefn maxWidth)) w
               hs = scanl' (\y -> (y-spacingV -).snd.dims.getAnn.snd) 0 w'
           in (
             layoutPickable <$> dragging
           , zipWith (\ h d -> second (modifyAnn (\bd->bd{position=(0,h)})) d) hs w'
           )

layoutPickable (PickDefn d,off) = (PickDefn (layoutDefn maxWidth d),off)
layoutPickable (PickExpr e,off) = (PickExpr (layoutExpr maxWidth e),off)


layoutDefn :: Float -> DefnBD -> DefnBD
layoutDefn maxWidth (Defn xd lns) = Defn xd{dims=sz} laidLines
  where
    sizedLines = map (layoutLine (maxWidth-2*paddingH)) lns
    szs = map (dims.getAnn) sizedLines
    ps = scanl' (\(x,y) (w,h) -> (x,y-h-spacingV)) (paddingH,-paddingV) szs
    sz = (maximum (map fst szs), -snd (last ps) - spacingV) + (2*paddingH,paddingV)
    laidLines = zipWith (\ pos -> modifyAnn (\bx->bx{position=pos}) ) ps sizedLines

newtype State s a  = State {runState :: (s-> (a,s))}
instance Functor (State s) where
  fmap f st = State (\s -> first f (runState st s))
instance Applicative (State s) where
  pure x = State  ((,) x)
  f <*> xm = State (\ s -> let (f',s') = runState f s in first f' (runState xm s))
instance Monad (State s) where
  return = pure
  xm >>= f = State (\s -> let (x,s') = runState xm s in runState (f x) s')
pop :: State [a] a
pop = State (\ (x:xs) -> (x,xs))
push :: a -> State [a] ()
push x = State (\ xs -> ((),x:xs))
peek :: State [a] a
peek = State (\ (x:xs) -> (x,x:xs))

layoutLine :: Float -> LineBD -> LineBD
layoutLine maxWidth ln = fst$ runState (onLine layoutVisitor ln) [maxWidth]

--layoutLine' :: LineBD -> State [Float] LineBD
--layoutLine' = onLine$ (setFilled layoutVisitorFilled' layoutVisitor')

layoutExpr :: Float -> ExprBD -> ExprBD
layoutExpr maxWidth e = fst$runState (onExpr layoutVisitor e) [maxWidth]

layoutVisitor = setFilled layoutVisitorFilled' layoutVisitor'

layoutVisitor' = scannerVis (const (peek >>= push.(subtract (2*paddingH)))) (\ xd chs -> do
  pop
  maxWidth <- peek
  let (txts,bxs,sz) = layoutThings maxWidth (texts xd) chs
  return$ (xd{texts=txts,dims=sz},bxs)
  )
layoutVisitorFilled' v (Filled xd e rs)= do
  -- don't need to interact with the stack beyond a peek
  maxWidth <- peek
  -- shouldn't have any text in
  e' <- onExpr v e
  return$ Filled xd{dims=dims (getAnn e')}  (modifyAnn (\d->d{position=(0,0)}) e') rs


layoutThings :: Float -> [(Int,TextBox)] -> [BoxData] -> ([(Int,TextBox)], [BoxData],(Float,Float))
layoutThings maxWidth txts bxs = (zip (map fst txts) tbs, bxs'  , sz)
    where
        parts = combine txts bxs
        partDims = map (getDims) (combine txts bxs)

        mrg :: ((Float, Float), (Float, Float)) -> (Float, Float) -> ((Float, Float), (Float, Float))
        mrg ((x,y),(prevWidth, lineheight)) (w,h) =
          if x+prevWidth+w <= maxWidth-paddingH || prevWidth==0
            then ((x+prevWidth,y), (w+spacingH,max lineheight h))
            else ((paddingH, y - lineheight - spacingV), (w,h)) -- new line
        positions = map fst (tail $ scanl' mrg ((paddingH,-paddingV),(0,0)) partDims)
        -- lns :: [[((Float,Float),Either TextBox (bx BoxData) )]]
        lns = groupBy ((==) `on` (snd . fst)) (zip positions parts)
        heights :: [Float]
        heights = map (maximum .(0:). map (snd.getDims.snd)) lns
        positionedBoxes =  concat $ zipWith
          (\l h -> map (\((left,top),bx) -> setPos (left,top - (h-snd (getDims bx))/2 ) bx) l )
          lns heights
        sz = foldr (both2 max) (0,0) (map (\bx-> (1,-1)*getPos bx + getDims bx  ) positionedBoxes) + (paddingH,paddingV)
        (tbs,bxs') = partitionEithers positionedBoxes



combine :: [(Int,a)] -> [b] -> [Either a b]
combine [] ys = map Right ys
combine ((0,x):xs) ys = Left x : combine xs ys
combine ((n,x):xs) (y:ys) = Right y : combine ((n-1,x):map (first (subtract 1)) xs) ys
combine (txt:_) [] = error "unpaired lists (not enough boxes to match text)"

getDims :: Either TextBox BoxData -> (Float,Float)
getDims = either textDims dims
getPos :: Either TextBox BoxData -> (Float,Float)
getPos = either tPos position
setPos :: (Float,Float) -> Either TextBox BoxData -> Either TextBox BoxData
setPos pos = bimap (\ tb -> tb{tPos=pos}) (\bd->bd{position=pos})


both2 :: (a->b->c)->(a,a)->(b,b)->(c,c)
both2 f (x1,y1) (x2,y2)  = (f x1 x2, f y1 y2)


draw :: World -> Float -> Picture
draw (mp,w) t = {-up$-} gTransform $ pictures (reverse $ map drawPickable (toList mp) ++ map (drawDefn.snd) w)

drawDefn :: DefnBD -> Picture
drawDefn = fst . onDefn drawVisitor

drawPickable :: (Pickable,a) -> Picture
drawPickable (PickDefn d,_) = fst $ onDefn drawVisitor d
drawPickable (PickExpr e,_) = fst $ onExpr drawVisitor e

drawVisitor' = visitVis (\ x ps -> (uncurry translate (position x) (Pictures (drawBoxData x: ps)), x) )

drawHole :: BoxVisitor ((,) Picture) BoxData BoxData -> HoleBD -> (Picture,HoleBD)
drawHole v (Filled xd e rs) = let (p, e') = onExpr v e in
              (uncurry translate (position xd) p, Filled xd e' rs)  -- don't draw the hole
drawHole v x = onHole' drawVisitor' v x
drawVisitor = drawVisitor'{
  onHole' = drawHole
}
markCols :: MarkState -> (Col,Col) -> (Col,Col)
markCols Unmarked cs = cs
markCols Marked _ = (makeColor 0.9 0.1 0.1 1,makeColor 0.6 0.0 0.0 1)
markCols _ _  =(makeColor 0.9 0.9 0.9 1,makeColor 0.6 0.0 0.0 1)

drawBoxData :: BoxData -> Picture
drawBoxData (BD{texts,dims,cols,outerShape,mark}) = let bx= scale unitScale unitScale $ drawBox (markCols mark cols) (dims/(unitScale,unitScale)) outerShape in
  Pictures (bx: map (drawText.snd) texts)

drawBoxData' :: BoxData -> Picture
drawBoxData' (BD{texts,dims,cols,outerShape}) = let bx= color (snd cols)$polygon (rectLR dims) in
  Pictures (bx: map (drawText.snd) texts)

rectLR :: (Float,Float) -> [(Float,Float)]
rectLR (w,h) = [(0,0),(w,0),(w,-h),(0,-h)]

drawText :: TextBox -> Picture
drawText TextBox{tText,tPos} = uncurry translate tPos (text tText)



