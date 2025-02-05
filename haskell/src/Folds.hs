module Folds where
import Types
import Control.Arrow
import Utils

incDepthsExpr :: ExprBD -> ExprBD
incDepthsExpr e = incDepthsExprN 0 e
incDepthsHole :: HoleBD -> HoleBD
incDepthsHole e = incDepthsHoleN 0 e

incDepthsExprN :: Int -> ExprBD -> ExprBD
incDepthsExprN n (Symbol xd hs) = Symbol (incDepth n xd) (map (incDepthsHoleN (n+1)) hs)
incDepthsExprN _ _ = error "TODO: add support for more programs"

incDepthsHoleN :: Int -> HoleBD -> HoleBD
incDepthsHoleN n (Hole xd chs) = Hole (incDepth n xd) (map (incDepthsExprN (n+1)) chs)
incDepthsHoleN n (Filled xd c chs) = Filled (incDepth n xd) (incDepthsExprN (n+1) c) (map (incDepthsExprN (n+1)) chs)

incDepth :: Int -> BoxData -> BoxData
incDepth n d = d{scope = fmap (\sc -> if sc<=n then sc+1 else sc) (scope d) }

--     where incDepthsN :: Int -> BoxTree -> BoxTree
--           incDepthsN n (Node lab chs) = let newScope= fmap (\sc -> if sc<=n then sc+1 else sc) (scope lab)
--                                         in Node lab{scope = newScope} (map (incDepthsN (n+1)) chs)

-- implicts = [PVar undefined . Ident undefined $ "imp_"++show i | i<-[0..]]

--Example traversals that I want to be able to replicate

data BoxVisitor m a b = BoxVisitor {
    onDefn' :: BoxVisitor m a b -> DefnBox a -> m (DefnBox b),
    onLine' :: BoxVisitor m a b -> LineBox a -> m (LineBox b),
    onLHS' :: BoxVisitor m a b -> LHSBox a -> m (LHSBox b),
    onPattern' :: BoxVisitor m a b -> PatternBox a -> m (PatternBox b),
    onLHSHole' :: BoxVisitor m a b -> LHSHoleBox a -> m (LHSHoleBox b),
    onLHSAntiHole' :: BoxVisitor m a b -> LHSAntiHoleBox a -> m (LHSAntiHoleBox b),
    onHole' :: BoxVisitor m a b -> HoleBox a -> m (HoleBox b),
    onExpr' :: BoxVisitor m a b -> ExprBox a -> m (ExprBox b)
}
setFilled onHoleFilled bv = bv{onHole' = \ v h -> case h of
  (Filled xd h hs) -> onHoleFilled v (Filled xd h hs)
  other -> onHole' bv v other
}

onDefn v = onDefn' v v
onLine v = onLine' v v
onLHS v = onLHS' v v
onPattern v = onPattern' v v
onLHSHole v = onLHSHole' v v
onLHSAntiHole v = onLHSAntiHole' v v
onHole v = onHole' v v
onExpr v = onExpr' v v

-- traverse, but you can partially overwrite it's behavior
recursingVisitor :: Applicative m => (a->m b) -> BoxVisitor m a b
recursingVisitor f = BoxVisitor{
    onDefn' = \ v (Defn xd args) -> Defn <$> f xd <*> (traverse (onLine v) args)
  , onLine' = \ v (Line xd lhs rhs) -> Line <$> f xd <*> onLHS v lhs <*> onHole v rhs
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

{-
visitHoleReader :: BoxVisitor ((->) k) a a -> HoleBox a -> k -> HoleBox a
visitHoleReader v (Filled x e chs) y = Filled x (onExpr v e y) (map (\ bx -> onExpr v bx y) chs)

visitHole :: Applicative m => BoxVisitor m a a -> HoleBox a -> m (HoleBox a)
visitHole v (Filled x e chs) = (Filled x <$> onExpr v e) <*> traverse (onExpr v) chs

visitDefn :: Monad m => BoxVisitor m a b -> Defn a -> m (Defn b)
visitDefn v d = return ()
visitLine :: BoxVisitor m a b -> LineBox a -> m (LineBox b)
visitLHS :: Monad BoxVisitor m a b -> LHS a -> m (LHS b)
visitPattern :: Monad BoxVisitor m a b -> Pattern a -> m (Pattern b)
visitLHSHole :: Monad BoxVisitor m a b -> LHSHole a -> m (LHSHole b)
visitLHSAntiHole :: Monad BoxVisitor m a b -> LHSAntiHole a -> m (LHSAntiHole b)
visitHole :: Monad BoxVisitor m a b -> Hole a -> m (Hole b)
visitExpr :: Monad BoxVisitor m a b -> Expr a -> m (Expr b)


-}
-- (forall bx. Ann bx => bx a -> m (bx b)) -> BoxVisitor m a b
visitTree :: (a -> [c] -> (c,b)) -> BoxVisitor ((,) c) a b --Applicative m => (a->m b) -> BoxVisitor m a b
visitTree f = BoxVisitor{
    onDefn' = \ v (Defn x lns) -> let (cs, lns') = unzip$ map (onLine v) lns in second (\y -> Defn y lns') (f x cs)
  , onLine' = \ v (Line x arg1 arg2) -> let ((c1,a1),(c2,a2)) = ((onLHS v) arg1, (onHole v) arg2) in second (\y -> Line y a1 a2) (f x [c1,c2])
  , onLHS' = \ v (Operator x args) -> let (cs, args') = unzip$ map (onPattern v) args in second (\y -> Operator y args') (f x cs)
  , onPattern' = \ v (Var x args) -> let (cs, args') = unzip$ map (onLHSAntiHole v) args in second (\y -> Var y args') (f x cs)
  , onLHSHole' = \ v (LHSHole x args) -> let (cs, args') = unzip$ map (onLHSAntiHole v) args in second (\y -> LHSHole y args') (f x cs)
  , onLHSAntiHole' = \ v (LHSAntiHole x args) -> let (cs, args') = unzip$ map (onLHSHole v) args in second (\y -> LHSAntiHole y args') (f x cs)
  , onHole' = visitTreeHole f
  , onExpr' = \ v (Symbol x args) -> let (cs, args') = unzip$ map (onHole v) args in second (\y -> Symbol y args') (f x cs)
  }
visitTreeHole :: (a -> [c] -> (c,b)) -> BoxVisitor ((,) c) a b -> HoleBox a -> (c,HoleBox b)
visitTreeHole f v (Hole x args) = let (cs, args') = unzip$ map (onExpr v) args in second (\y -> Hole y args') (f x cs)
visitTreeHole f v (Filled x arg args) =  let (cs, (arg':args')) = unzip$ map (onExpr v) (arg:args) in
                                        second (\y -> Filled y arg' args') (f x cs)

visitHoleVis :: (a -> [c] -> (c,a)) -> BoxVisitor ((,) c) a a -> HoleBox a -> (c,HoleBox a)
visitHoleVis f v (Hole x args) = let (cs, args') = unzip$ map (onExpr v) args in second (\y -> Hole y args') (f x cs)
visitHoleVis  f v (Filled x arg args) =  let (c, arg') = onExpr v arg in
                                        second (\y -> Filled y arg' args) (f x [c])
-- | Visit visible nodes in a tree
visitVis :: (a -> [c] -> (c,a)) -> BoxVisitor ((,) c) a a
visitVis f = (visitTree f){onHole' = visitHoleVis f}

scanner :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b
scanner stepIn f = BoxVisitor{
    onDefn' = scanDefn stepIn f
  , onLine' = scanLine stepIn f
  , onLHS' = scanLHS stepIn f
  , onPattern' = scanPattern stepIn f
  , onLHSHole' = scanLHSHole stepIn f
  , onLHSAntiHole' = scanLHSAntiHole stepIn f
  , onHole' = scanHole stepIn f
  , onExpr' = scanExpr stepIn f
  }
{-
scanExpr :: Monad m => (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> ExprBox a -> m (ExprBox b)
scanExpr f v (Symbol xd hs) = do
  hs' <- traverse (onHole v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Symbol xd' (zipWith setAnn xds hs')-}


-- The creation of these involved some copy and pasting. How should this be accomplished using TypeBlocks?
-- It could probably be done using Generics intelligently, but that's a bit opaque and adds more details for me to worry about
--   Some sort of macros that allow attempting evaluation of untyped terms?

scanDefn :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> DefnBox a -> m (DefnBox b)
scanDefn stepIn f v (Defn xd hs) = do
  stepIn xd
  hs' <- traverse (onLine v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Defn xd' (zipWith setAnn xds hs')

scanLine :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> LineBox a -> m (LineBox b)
scanLine stepIn f v (Line xd lhs rhs) = do
  stepIn xd
  lhs' <- onLHS v lhs
  rhs' <- onHole v rhs
  result <- f xd [getAnn lhs', getAnn rhs']
  let (xd',[lxd,rxd]) = result
  return $ Line xd' (setAnn lxd lhs') (setAnn rxd rhs')

scanLHS :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> LHSBox a -> m (LHSBox b)
scanLHS stepIn f v (Operator xd hs) = do
  stepIn xd
  hs' <- traverse (onPattern v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Operator xd' (zipWith setAnn xds hs')

scanPattern :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> PatternBox a -> m (PatternBox b)
scanPattern stepIn f v (Var xd hs) = do
  stepIn xd
  hs' <- traverse (onLHSAntiHole v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Var xd' (zipWith setAnn xds hs')

scanLHSHole :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> LHSHoleBox a -> m (LHSHoleBox b)
scanLHSHole stepIn f v (LHSHole xd hs) = do
  stepIn xd
  hs' <- traverse (onLHSAntiHole v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ LHSHole xd' (zipWith setAnn xds hs')

scanLHSAntiHole :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> LHSAntiHoleBox a -> m (LHSAntiHoleBox b)
scanLHSAntiHole stepIn f v (LHSAntiHole xd hs) = do
  stepIn xd
  hs' <- traverse (onLHSHole v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ LHSAntiHole xd' (zipWith setAnn xds hs')

scanHole :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> HoleBox a -> m (HoleBox b)
scanHole stepIn f v (Hole xd hs) = do
  stepIn xd
  hs' <- traverse (onExpr v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Hole xd' (zipWith setAnn xds hs')
scanHole stepIn f v (Filled xd h hs) = do
  stepIn xd
  r1 <- traverse (onExpr v) (h:hs)
  let (h':hs') = r1
  result <- f xd (map getAnn (h':hs'))
  let (xd',hxd:xds) = result
  return $ Filled xd' (setAnn hxd h') (zipWith setAnn xds hs')

scanExpr :: Monad m => (a -> m ()) -> (a -> [b] -> m (b,[b])) -> BoxVisitor m a b -> ExprBox a -> m (ExprBox b)
scanExpr stepIn f v (Symbol xd hs) = do
  stepIn xd
  hs' <- traverse (onHole v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Symbol xd' (zipWith setAnn xds hs')


scanHoleVis :: Monad m => (a -> m ()) -> (a -> [a] -> m (a,[a])) -> BoxVisitor m a a -> HoleBox a -> m (HoleBox a)
scanHoleVis stepIn f v (Hole xd hs) = do
  stepIn xd
  hs' <- traverse (onExpr v) hs
  (xd',xds) <- f xd (map getAnn hs')
  return $ Hole xd' (zipWith setAnn xds hs')
scanHoleVis stepIn f v (Filled xd h hs) = do
  stepIn xd
  h' <- onExpr v h
  result <- f xd [getAnn h']
  let (xd',[hxd]) = result
  return $ Filled xd' (setAnn hxd h') hs

-- traverse only visible parts of the tree
scannerVis stepIn f = (scanner stepIn f){onHole'=scanHoleVis stepIn f}
