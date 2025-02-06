{-# LANGUAGE RankNTypes #-}
module Interact where

import Graphics.Gloss.Interface.Pure.Game (Event(..),Key(..),SpecialKey(..),MouseButton(..))

import Paths
import Folds
import Types
import Control.Monad.Trans.Reader


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

{-
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
-}

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
{-
-- Do something with the particular node being clicked on
withClicked :: Applicative m => BoxVisitor m a a -> BoxVisitor (ReaderT (Float,Float) m) a a
withClicked onClicked = BoxVisitor{
    onDefn' = \ v (Defn xd args) -> reader (\pt -> case inBoxes pt getAnn args of
      Nothing -> onDefn onClicked (Defn xd args)
      Just (ls,bx,pt',rs) -> runReader (Defn xd . (\ x -> ls++ (x:rs)) <$> onLine v bx) pt'
      )
  , onLine' = \ v (Line xd lhs rhs) -> reader (\ pt -> case inBox pt (getAnn lhs) of
      Just pt' -> (\ l' -> Line xd l' rhs) <$> runReader (onLHS v lhs) pt'
      Nothing -> case inBox pt (getAnn rhs) of
          Just pt' -> runReader (Line xd lhs <$> onExpr v rhs) pt'
          Nothing -> onLine onClicked (Line xd lhs rhs)
          )
  , onLHS' = \ v (Operator xd args) -> reader (\pt -> case inBoxes pt getAnn args of
      Nothing -> onLHS onClicked (Operator xd args)
      Just (ls,bx,pt',rs) -> runReader (Operator xd . (\ x -> ls++(x:rs)) <$> onPattern v bx) pt'
      )
  , onPattern' = \ v pat -> case pat of
      Var xd args -> reader (\pt -> case inBoxes pt getAnn args of
        Nothing -> onPattern onClicked (Var xd args)
        Just (ls,bx,pt',rs) -> runReader (Var xd . (\ x -> ls++(x:rs)) <$> onLHSAntiHole v bx) pt'
        )
      Constructor xd args -> reader (\pt -> case inBoxes pt getAnn args of
        Nothing -> onPattern onClicked (Constructor xd args)
        Just (ls,bx,pt',rs) -> runReader (Constructor xd . (\ x -> ls++(x:rs)) <$> onPattern v bx) pt'
        )
  , onLHSHole' = \ v (LHSHole xd args) -> reader (\pt -> case inBoxes pt getAnn args of
        Nothing -> onLHSHole onClicked (LHSHole xd args)
        Just (ls,bx,pt',rs) -> runReader (LHSHole xd . (\ x -> ls++(x:rs)) <$> onLHSAntiHole v bx) pt'
        )
  , onLHSAntiHole' = \ v (LHSAntiHole xd args) -> reader (\pt -> case inBoxes pt getAnn args of
        Nothing -> onLHSAntiHole onClicked (LHSAntiHole xd args)
        Just (ls,bx,pt',rs) -> runReader (LHSAntiHole xd . (\ x -> ls++(x:rs)) <$> onLHSHole v bx) pt'
        )
  , onHole' = \ v h -> case h of
      Hole xd args -> reader (\pt -> case inBoxes pt getAnn args of
        Nothing -> onLHSHole onClicked (Hole xd args)
        Just (ls,bx,pt',rs) -> runReader (Hole xd . (\ x -> ls++(x:rs)) <$> onExpr v bx) pt'
        )
      Filled xd a args -> Filled <$> f xd <*> onExpr v a <*> (traverse (onExpr v) args)
  , onExpr' = \ v (Symbol xd args) -> Symbol <$> f xd <*> (traverse (onHole v) args)
}

 -}

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
{-
f1 ::Monad mm => BoxVisitor mm BoxData BoxData -> BoxData->(d->BoxData)->[d]->(d->ReaderT (Float,Float) mm d)->(BoxVisitor mm BoxData BoxData -> c -> mm c)->(BoxData->[d]->c)-> ReaderT (Float,Float) mm c
f1 onClicked xd gt args recurse onThing construct = construct xd <$> (traverse recurse args) -}
withClicked' :: Functor mm => BoxVisitor mm BoxData BoxData -> BoxData->(d->BoxData)->[d]->(d-> ReaderT (Float,Float) mm d)->(BoxVisitor mm BoxData BoxData -> c -> mm c)->(BoxData->[d]->c)-> ReaderT (Float,Float) mm c
withClicked' onClicked xd gt args recurse onThing construct = ReaderT (\ pt -> case inBoxes pt gt args of
      Nothing -> onThing onClicked (construct xd args)
      Just (ls,bx,pt',rs) -> runReaderT (construct xd . (\ x -> ls++(x:rs)) <$> recurse bx) pt'
      )
-- test :: Functor m => BoxVisitor m BoxData BoxData -> BoxVisitor (ReaderT (Float,Float) m) BoxData BoxData
-- test onClicked = commonCaseFold undefined (f2 onClicked)
