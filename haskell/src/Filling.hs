module Filling(tryToFill) where
import Types
import Control.Monad.Trans.State



unmarked :: Ann bx => bx BoxData -> Bool
unmarked bx = mark (getAnn bx) == Unmarked

-- Fill Hole with Expr
tryToFill :: HoleBD -> ExprBD -> Maybe HoleBD
tryToFill (Hole xd args) e = if baseType (getAnn e) /= baseType xd then Nothing else
  let (body, spare) = runState (fillExpr e) (filter unmarked args) in Just $ Filled xd (body) spare


fillExpr :: ExprBD -> State [ExprBD] ExprBD
fillExpr (Symbol xd hs) = Symbol xd <$> mapM fillHole hs

fillHole :: HoleBD -> State [ExprBD] HoleBD
fillHole (Filled xd arg rs) = if unmarked arg then (\arg' -> Filled xd arg' rs) <$> fillExpr arg
                                              else fillHole (Hole xd rs)
fillHole h = state (\ args -> case args of
  [] -> (h,args)
  (arg:rs) -> maybe (h,args) (\x->(x,rs)) (tryToFill h (head args))
  )
