{-# LANGUAGE RankNTypes #-}
module Interact where

import Graphics.Gloss.Interface.Pure.Game hiding (Picture(..))

import Paths
import Graphics
import Folds
import Types
import Control.Monad.Trans.Reader
import Control.Arrow
import Data.Functor.Identity
import GlobalTransform

--import Utils


handleEvent :: Event -> [(String, DefnBD)] -> [(String, DefnBD)]
-- TODO: Implement this
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) w = let pos' = gInvert pos in
  map (second (\d -> case pos' `inBox` getAnn d of
   Just pos'' -> runIdentity$ runReaderT (onDefn (withClicked makeRed) d) pos''
   Nothing -> d) ) w
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) w = let pos' = gInvert pos in
  map (second (\d -> runIdentity $ onDefn makeWhite d) ) w
handleEvent e w = w

makeRed :: BoxVisitor Identity BoxData BoxData
makeRed = recursingVisitor (\ xd -> return xd{cols=(makeColor 0.9 0.1 0.1 1,makeColor 0.6 0.0 0.0 1)})
makeWhite :: BoxVisitor Identity BoxData BoxData
makeWhite= recursingVisitor (\ xd -> return xd{cols=(makeColor 0.9 0.9 0.9 1,makeColor 0.6 0.0 0.0 1)})


inBD :: (Float,Float) -> BoxData -> Bool
inBD pt xd = inRect (fst pt,-snd pt) ((-4)*(unitScale,unitScale)) (dims xd + 4*(unitScale,unitScale)) &&  inFirstPicture pt (drawBoxData xd)

first4 :: (a->e) -> (a,b,c,d) -> (e,b,c,d)
first4  f (a,b,c,d) = (f a,b,c,d)

inBox :: (Float, Float) -> BoxData -> Maybe (Float,Float)
inBox pt xd = let pt' = pt - position xd in
                  if pt' `inBD` xd then Just pt' else Nothing

-- | Find out if a point is somewhere in a list of boxes
-- Return a zipper to the box it's in, alongside the scaled point
inBoxes :: (Float,Float) -> (a -> BoxData) -> [a] -> Maybe ([a],a,(Float,Float),[a])
inBoxes pt getBD (bx:bxs) = let xd = getBD bx in
    maybe
      (fmap (first4 (bx:)) (inBoxes pt getBD bxs))
      (\pt' -> Just ([],bx, pt',bxs))
      (pt `inBox` xd)
inBoxes pt getBD [] = Nothing

withClicked :: Applicative m => BoxVisitor m BoxData BoxData -> BoxVisitor (ReaderT (Float,Float) m) BoxData BoxData
withClicked onClicked = commonCaseFold (withClicked' onClicked) (BoxVisitor{
  onLine' = \ v (Line xd lhs rhs) -> ReaderT (\ pt -> case inBox pt (getAnn lhs) of
      Just pt' -> (\ l' -> Line xd l' rhs) <$> runReaderT (onLHS v lhs) pt'
      Nothing -> case inBox pt (getAnn rhs) of
          Just pt' -> runReaderT (Line xd lhs <$> onHole v rhs) pt'
          Nothing -> onLine onClicked (Line xd lhs rhs)
          )
  , onHole' = \ v (Filled xd a args) -> ReaderT  (\pt -> case inBox pt (getAnn a) of
        Nothing -> onHole onClicked (Filled xd a args)
        Just pt' -> runReaderT ((\ x -> Filled xd x args) <$> onExpr v a) pt'
        )
  , onExpr' = undefined
  , onDefn' = undefined
  , onPattern' = undefined
  , onLHS' = undefined
  , onLHSHole' = undefined
  , onLHSAntiHole' = undefined
  })

-- | specify behaviour on lines and filled holes
allCasesCommonFold :: (forall c d.
    a->(d->a)->[d]->(d->m d)->(BoxVisitor n a a -> c -> n c)->(a->[d]->c)-> m c) ->
  (BoxVisitor m a a -> LineBox a -> m (LineBox a)) ->
  (BoxVisitor m a a -> HoleBox a -> m (HoleBox a))
    -> BoxVisitor m a a
allCasesCommonFold f onLn onH = commonCaseFold f (BoxVisitor{
    onLine' = onLn
  , onHole' = onH
  , onExpr' = undefined
  , onDefn' = undefined
  , onPattern' = undefined
  , onLHS' = undefined
  , onLHSHole' = undefined
  , onLHSAntiHole' = undefined
  })

-- Visit nodes that consist entirely of a a list
-- Does not cover lines, filled holes, or fancy expressions
commonCaseFold :: (forall c d. a->(d->a)->[d]->(d->m d)->(BoxVisitor n a a -> c -> n c)->(a->[d]->c)-> m c) -> BoxVisitor m a a -> BoxVisitor m a a
commonCaseFold f vv = vv{
    onDefn' = \ v (Defn xd args) -> f xd getAnn args (onLine v) onDefn Defn
  , onLHS' = \ v (Operator xd args) -> f xd getAnn args (onPattern v) onLHS Operator
  , onPattern' = \ v pat -> case pat of
    (Var xd args) -> f xd getAnn args (onLHSAntiHole v) onPattern Var
    (Constructor xd args) -> f xd getAnn args (onPattern v) onPattern Constructor
  , onLHSHole' = \ v (LHSHole xd args) -> f xd getAnn args (onLHSAntiHole v) onLHSHole LHSHole
  , onLHSAntiHole' = \ v (LHSAntiHole xd args) -> f xd getAnn args (onLHSHole v) onLHSAntiHole LHSAntiHole
  , onHole' = \ v h -> case h of
      (Hole xd args) -> f xd getAnn args (onExpr v) onHole Hole
      other -> onHole' vv v other
  , onExpr' = \ v e -> case e of
      (Symbol xd args) -> f xd getAnn args (onHole v) onExpr Symbol
      other -> onExpr' vv v other
}

withClicked' :: Functor mm => BoxVisitor mm BoxData BoxData
  -> BoxData
  ->(d->BoxData)
  ->[d]
  ->(d-> ReaderT (Float,Float) mm d)
  ->(BoxVisitor mm BoxData BoxData -> c -> mm c)->(BoxData->[d]->c)
    -> ReaderT (Float,Float) mm c
withClicked' onClicked xd gt args recurse onThing construct = ReaderT (\ pt -> case inBoxes pt gt args of
      Nothing -> onThing onClicked (construct xd args)
      Just (ls,bx,pt',rs) -> runReaderT (construct xd . (\ x -> ls++(x:rs)) <$> recurse bx) pt'
      )
