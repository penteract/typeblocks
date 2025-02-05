module Interact where

import Graphics.Gloss.Interface.Pure.Game (Event(..),Key(..),SpecialKey(..),MouseButton(..))

import Paths
import Folds
import Types


handleEvent :: Event -> [(String, DefnBD)] -> [(String, DefnBD)]
-- TODO: Implement this
handleEvent e w = w

inBD :: (Float,Float) -> BoxData -> Bool
inBD pt xd = inShape pt (dims xd) (outerShape xd)

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

withClickedDefn :: Functor m => BoxVisitor m BoxData BoxData -> (Float,Float) -> DefnBox BoxData -> m (DefnBox BoxData)
withClickedDefn onClicked pt (Defn xd args) = case inBoxes pt getAnn args of
      Nothing -> onDefn onClicked (Defn xd args)
      Just (ls,bx,pt',rs) -> Defn xd . (\ x -> ls ++ (x:rs)) <$> withClickedLine onClicked pt' bx

withClickedLine onClicked pt (Line xd lhs rhs) = case inBox pt (getAnn lhs) of
    Just pt' -> (\ l' -> Line xd l' rhs) <$> withClickedHole onClicked pt' lhs
    Nothing -> case inBox pt (getAnn rhs) of
        Just pt' -> Line xd lhs <$> withClickedExpr onClicked pt' rhs
        Nothing -> onLine onClicked (Line xd lhs rhs)
withClickedHole = undefined
withClickedExpr = undefined
{-
-- Do something with the particular node being clicked on
withClicked :: Applicative m => BoxVisitor m a a -> (Float,Float) -> BoxVisitor m a a
withClicked onClicked pt = BoxVisitor{
    onDefn' = \ v (Defn xd args) -> case inBoxes pt getAnn args of
      Nothing -> onDefn onClicked (Defn xd args)
      Just (ls,bx,pt',rs) -> Defn xd . (\ x -> ls++ (x:rs)) <$> onLine (withClicked onClicked pt') bx
  , onLine' = \ v (Line xd lhs rhs) -> case inBox pt (getAnn lhs) of
                                            Just pt' -> (\ l' -> Line xd l rhs) <$> onLHS (withClicked onClicked pt') lhs
                                            Nothing -> case in
  Line <$> f xd <*> onLHS v lhs <*> onHole v rhs
  , onLHS' = \ v (Operator xd args) -> Operator <$> f xd <*> (traverse (onPattern v) args)
  , onPattern' = \ v pat -> case pat of
      Var xd args -> Var <$> f xd <*> (traverse (onLHSAntiHole v) args)
      Constructor xd args -> Constructor <$> f xd <*> (traverse (onPattern v) args)
  , onLHSHole' = \ v (LHSHole xd args) -> LHSHole <$> f xd <*> (traverse (onLHSAntiHole v) args)
  , onLHSAntiHole' = \ v (LHSAntiHole xd args) -> LHSAntiHole <$> f xd <*> (traverse (onLHSHole v) args)
  , onHole' = \ v h -> case h of
      Hole xd args -> Hole <$> f xd <*> (traverse (onExpr v) args)
      Filled xd a args -> Filled <$> f xd <*> onExpr v a <*> (traverse (onExpr v) args)
  , onExpr' = \ v (Symbol xd args) -> Symbol <$> f xd <*> (traverse (onHole v) args)
}
-}
