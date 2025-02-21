{-# LANGUAGE RankNTypes #-}
module Interact where

import Graphics.Gloss.Interface.Pure.Game hiding (Picture(..))

import GlobalTransform
import Paths
import Graphics
import Folds
import Types
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Arrow
import Data.Functor.Identity
import Data.Monoid
import Control.Applicative
import Data.Maybe
import Control.Monad
import Data.Tuple

--import Utils


mark :: BoxData -> BoxData
mark xd = xd{marked=True}

unmark :: BoxData -> BoxData
unmark xd = xd{marked=False}

pickup :: (Float,Float) -> BoxVisitor (WriterT (First (Pickable,(Float,Float))) Maybe) BoxData BoxData
pickup off = BoxVisitor {
    onLine' = \v bx -> empty
  , onHole' = \v bx -> empty
  , onExpr' = \v bx -> tell (First$Just$ (PickExpr bx,off)) >> return (modifyAnn mark bx)
  , onDefn' = \ v bx -> tell (First$Just$ (PickDefn bx,off)) >> return (modifyAnn mark bx)
  , onPattern' = \v bx -> empty -- TODO (Allow altering constructors and moving variables into appropriate scopes)
  , onLHS' = \v bx -> empty -- TODO (make expr from LHS)
  , onLHSHole' = \v bx -> empty
  , onLHSAntiHole' = \v bx -> empty
  }

-- 'Nothing' means stop;  Just Nothing means look up a level
droop :: ExprBD -> (Float,Float) -> BoxVisitor (MaybeT Maybe) BoxData BoxData
droop e _ = BoxVisitor {
    onLine' = \v bx -> empty
  , onHole' = \v bx -> empty -- TODO: allow dropping expressions into holes
  , onExpr' = \v bx -> empty
  , onDefn' = \ v bx -> empty
  , onPattern' = \v bx -> empty
  , onLHS' = \v bx -> empty
  , onLHSHole' = \v bx -> empty
  , onLHSAntiHole' = \v bx -> empty
  }

withFirstClicked :: (DefnBD -> (Float,Float) -> Maybe (a,DefnBD)) -> (Float,Float) -> [(String,DefnBD)] -> (Maybe a,[(String,DefnBD)])
withFirstClicked fn pos ((n,d):ds) = case (pos `inBox` getAnn d) of
  Just pos' -> maybe (Nothing, (n,d):ds) (Just *** ((:ds) . (,) n)) (fn d pos')
  Nothing -> second ((n,d):) $ withFirstClicked fn pos ds
withFirstClicked fn pos [] = (Nothing,[])

--TODO: only do something once if there are multiple overlapping defns
handleEvent :: Event -> (Maybe (Pickable,(Float,Float)), [(String, DefnBD)]) -> (Maybe (Pickable,(Float,Float)), [(String, DefnBD)])
handleEvent (EventKey (MouseButton LeftButton) Down _ pos) (Nothing, ds) =
  handleEvent (EventMotion pos) $
  second (filter (not.marked.getAnn.snd)) $
  let pos' = gInvert pos in
      withFirstClicked (\ d -> ((\(d,First mx) -> (flip (,) d  <$> mx)) =<<) . runWriterT . runReaderT (onDefn (withClicked' pickup) d)) pos' ds

  {- mapM (\(n,d) ->
    case (pos' `inBox` getAnn d) >>= (runWriterT . runReaderT (onDefn (withClicked' pickup) d)) of
         Just (a,b) -> (b, (n,a))
         Nothing -> (mempty, (n,d)) ) ds
         -}
handleEvent (EventMotion pos) (Just (pk,off), ds) = let pos' = gInvert pos in
  (Just (setPickablePos (pos'-off) pk, off),  ds)
-- TODO: carefully consider whether the cursor should be at the corner of a box (making it easier to see where it's being placed) or retain the relative offset when the cursor was picked up (making the dragging motion more intuitive and less jumpy; substantially clearer when dragged zero distance)
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) (Just (PickExpr e,_), ds) = let pos' = gInvert pos in
     case withFirstClicked (\ defn pos'' -> (,) () <$> (join $ runMaybeT (runReaderT (onDefn (withClicked' (droop e)) defn) pos'')) ) pos' ds of
          (Nothing,ds') -> (Nothing, map (second (runIdentity . onDefn (recursingVisitor (return . unmark)))) ds)
          (Just _,ds') -> (Nothing,ds') -- Drop succeeded
{-
  map (\(n,d) ->
    case (pos' `inBox` getAnn d) >>= runMaybeT . runReaderT (onDefn (withClicked' (droop e)) d) of
         Just (Just x) -> (n,x)
         _ -> (n,d) ) ds) -}
handleEvent (EventKey (MouseButton LeftButton) Up _ pos) (Just (PickDefn d,off), ds) = (Nothing,(getName d,d) : ds)
handleEvent e w = w

getName :: DefnBD -> String
getName (Defn _ (Line _ (Operator xd _) _:_)) = tText $ snd (head (texts xd))

modAnnP f (PickDefn d) = PickDefn (modifyAnn f d)
modAnnP f (PickExpr d) = PickExpr (modifyAnn f d)
setPickablePos delta = modAnnP (\xd -> xd{position=(delta)})

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
withClicked onClicked = commonCaseFold (withClickedCommon onClicked) (BoxVisitor{
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

withClickedCommon :: Functor mm => BoxVisitor mm BoxData BoxData
  -> BoxData
  ->(d->BoxData)
  ->[d]
  ->(d-> ReaderT (Float,Float) mm d)
  ->(BoxVisitor mm BoxData BoxData -> c -> mm c)
  ->(BoxData->[d]->c)
    -> ReaderT (Float,Float) mm c
withClickedCommon onClicked xd gt args recurse onThing construct = ReaderT (\ pt -> case inBoxes pt gt args of
      Nothing -> onThing onClicked (construct xd args)
      Just (ls,bx,pt',rs) -> runReaderT (construct xd . (\ x -> ls++(x:rs)) <$> recurse bx) pt'
      )

-- Let a handler return 'Nothing' to bubble events back up
withClickedCommon' :: Alternative mm => ((Float,Float) -> BoxVisitor mm BoxData BoxData)
  -> BoxData
  -> (d->BoxData)
  -> [d]
  -> (d-> ReaderT (Float,Float) mm d)
  -> (BoxVisitor mm BoxData BoxData -> c -> mm c)
  -> (BoxData->[d]->c)
    -> ReaderT (Float,Float) mm c
withClickedCommon' onClicked xd gt args recurse onThing construct = ReaderT (\ pt -> case inBoxes pt gt args of
    Nothing -> onThing (onClicked pt) (construct xd args)
    Just (ls,bx,pt',rs) ->  runReaderT (construct xd . (\ x -> ls++(x:rs)) <$> recurse bx) pt'
        <|> onThing (onClicked pt) (construct xd args)
    )

withClicked' :: Alternative m => ((Float,Float) -> BoxVisitor m BoxData BoxData) -> BoxVisitor (ReaderT (Float,Float) m) BoxData BoxData
withClicked' onClicked = allCasesCommonFold (withClickedCommon' onClicked)
    (\ v (Line xd lhs rhs) -> ReaderT (\ pt -> case inBox pt (getAnn lhs) of
      Just pt' -> (\ l' -> Line xd l' rhs) <$> runReaderT (onLHS v lhs) pt' <|> onLine (onClicked pt) (Line xd lhs rhs)
      Nothing -> case inBox pt (getAnn rhs) of
          Just pt' -> runReaderT (Line xd lhs <$> onHole v rhs) pt' <|> onLine (onClicked pt) (Line xd lhs rhs)
          Nothing -> onLine (onClicked pt) (Line xd lhs rhs)
      ))
    (\ v (Filled xd a args) -> ReaderT (\pt -> case inBox pt (getAnn a) of
            Nothing -> onHole (onClicked pt) (Filled xd a args)
            Just pt' -> runReaderT ((\ x -> Filled xd x args) <$> onExpr v a) pt' <|> onHole (onClicked pt) (Filled xd a args)
      ))

