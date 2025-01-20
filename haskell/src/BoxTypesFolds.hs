module BoxTypesFolds where
import BoxTypes
import Control.Arrow

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

onDefn v = onDefn' v v
onLine v = onLine' v v
onLHS v = onLHS' v v
onPattern v = onPattern' v v
onLHSHole v = onLHSHole' v v
onLHSAntiHole v = onLHSAntiHole' v v
onHole v = onHole' v v
onExpr v = onExpr' v v

visitHoleReader :: BoxVisitor ((->) k) a a -> HoleBox a -> k -> HoleBox a
visitHoleReader v (Filled x e chs) y = Filled x (onExpr v e y) (map (\ bx -> onExpr v bx y) chs)

visitHole :: Applicative m => BoxVisitor m a a -> HoleBox a -> m (HoleBox a)
visitHole v (Filled x e chs) = (Filled x <$> onExpr v e) <*> traverse (onExpr v) chs
{-
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
