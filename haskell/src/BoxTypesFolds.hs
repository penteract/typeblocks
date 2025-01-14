module BoxTypesFolds where
import BoxTypes

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
    onDefn :: BoxVisitor m a b -> DefnBox a -> m (DefnBox b),
    onLHS :: BoxVisitor m a b -> LHSBox a -> m (LHSBox b),
    onPattern :: BoxVisitor m a b -> PatternBox a -> m (PatternBox b),
    onLHSHole :: BoxVisitor m a b -> LHSHoleBox a -> m (LHSHoleBox b),
    onLHSAntiHole :: BoxVisitor m a b -> LHSAntiHoleBox a -> m (LHSAntiHoleBox b),
    onHole :: BoxVisitor m a b -> HoleBox a -> m (HoleBox b),
    onExpr :: BoxVisitor m a b -> ExprBox a -> m (ExprBox b)
}


visitHoleReader :: BoxVisitor ((->) k) a a -> HoleBox a -> k -> HoleBox a
visitHoleReader v (Filled x e chs) y = Filled x (onExpr v v e y) (map (\ bx -> onExpr v v bx y) chs)

visitHole :: Applicative m => BoxVisitor m a a -> HoleBox a -> m (HoleBox a)
visitHole v (Filled x e chs) = (Filled x <$> onExpr v v e) <*> traverse (onExpr v v) chs
{-
visitDefn :: Monad m => BoxVisitor m a b -> Defn a -> m (Defn b)
visitDefn v d = return ()
visitLHS :: Monad BoxVisitor m a b -> LHS a -> m (LHS b)
visitPattern :: Monad BoxVisitor m a b -> Pattern a -> m (Pattern b)
visitLHSHole :: Monad BoxVisitor m a b -> LHSHole a -> m (LHSHole b)
visitLHSAntiHole :: Monad BoxVisitor m a b -> LHSAntiHole a -> m (LHSAntiHole b)
visitHole :: Monad BoxVisitor m a b -> Hole a -> m (Hole b)
visitExpr :: Monad BoxVisitor m a b -> Expr a -> m (Expr b)


-}
