module BoxTypesParsing where
--import Data.Tree
import BoxTypes
import BoxTypesFolds
import qualified BoxTypes as BX
import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Language.Haskell.Exts.Syntax as HS
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Pretty
import Data.List
import Control.Arrow
import qualified Data.Map as Map

-- possibly I should use the GHC API, but that's a pain to use
-- (I gave up after some error about platform constants not found)
import Language.Haskell.Interpreter hiding(Extension(..))


import System.IO.Unsafe(unsafePerformIO)
sup :: Show a => a -> b -> b
sup = seq . unsafePerformIO . print

data BindTarget = DefnBox DefnBD | Builtin ExprBD | Local ExprBD deriving Show
-- Not the state monad, because when we add things, we usually only want to apply it to subparts and don't want to mutate it when returning
type Env a = [(String,BindTarget)] -> a

getExpr :: String -> Env ExprBD
getExpr nm env = case lookup nm env of
                      Just (Local e) -> e
                      Just (Builtin e) -> e
                      Just (DefnBox (Defn _ (Line _ lhs _ :_))) -> lhsToExpr lhs
                      Just (x) -> error (show x)
                      Nothing -> error (show (nm,env))

type ModCache = Map.Map String [(String,DefnBD)]

moduleToBoxes :: Language.Haskell.Exts.Syntax.Module l -> ModCache -> IO (ModCache,[(String,DefnBD)])
moduleToBoxes (Module _ _ _ imps ds) knownModules = do
    (c', env)<- getImps (preludeDecl:imps) knownModules []
    return (c',
        zipWith (\ n -> second (modifyAnn (\bx-> bx{position=(0,-20*n)})) ) [0..] (dsToBoxes ds env))

preludeDecl = ImportDecl undefined (ModuleName undefined "Prelude") undefined undefined undefined undefined undefined undefined

getImps :: [ImportDecl l]-> ModCache -> Env (IO (ModCache,[(String,BindTarget)]))
getImps [] cache env = return (cache,env)
getImps (dec:decs) cache env =
    let ModuleName _ modName = importModule dec in
        do
            Right imps <- runInterpreter (loadImp modName)
            --print imps
            let boxImps = [] -- map (second typeToAntihole) imps
            getImps decs (Map.insert modName boxImps cache) (boxImps ++ env)



loadImp :: String -> Interpreter [(String,Type)]
loadImp importName = do
    exs <- get languageExtensions
    vrs <- getModuleExports importName
    setImports [importName]
    concat <$> mapM getType vrs

getType :: ModuleElem -> Interpreter [(String,Type)]
getType (Fun nm) = parseTypeWithMode (defaultParseMode{extensions=[EnableExtension FlexibleContexts]}) <$> typeOf nm >>= (\ x ->
    case x of
        (ParseOk ty) -> return [(nm,toType ty)]
        other -> error ("name: "++nm++"\n"++show other)
    )
getType (Class _ xs) = concat <$> mapM (getType . Fun) xs
getType (Data _ xs) = concat <$> mapM (getType . Fun) xs


-- x,y :: Int (multiple type signatures on one line is possible)

dsToBoxes :: [Decl l] -> Env [(String,DefnBD)]
dsToBoxes ds env = [(name, defnToBox (typ . getAnn =<< lookup name typeSigs) clauses (map (second (Local . lhsAHtoExpr)) typeSigs ++ env)) | (name,clauses) <- defns]
    where
        typeSigs = [(prettyPrint n, addText (0,prettyPrint n) $ typeToAntihole (toType t)) | (TypeSig _ ns t) <- ds, n<-ns]
        defns = [ (getName (head ms), ms) | FunBind _ ms <- ds]


getName :: Match l -> String
getName (Match _ nm _ _ _) = prettyPrint nm
getName (InfixMatch _ _ nm _ _ _) = prettyPrint nm

-- Adding polymorphism will be fun
toType :: HS.Type l -> Type
toType (TyParen _ t) = toType t
toType (TyCon _ t) = Base (prettyPrint t)
toType (TyFun _ a b) = toType a :-> toType b
--TODO: parse these properly
toType (TyForall _ _ _ r) = toType r
toType (TyApp _ a b) = toType b
toType (TyVar _ t) = Base (prettyPrint t)
toType (TyList _ a) = toType a
toType (TyTuple _ _ ts) = Base (ts>>=prettyPrint)
toType other = error ("unknown toType:" ++ show (return () <$> other))

defnToBox :: Maybe Type -> [Match l] -> Env DefnBD
defnToBox t lines = defnBox <$> (mapM (lineToBox t) lines)

lineToBox :: Maybe Type -> Match l -> Env LineBD --(LHSBD,HoleBD)
lineToBox (Just t) (InfixMatch _ l symb rs rhs w) env = undefined
lineToBox (Just t) (Match _ symb args (UnGuardedRhs _ rhsExpr) w) env = lineBox lhsWithName rhsBox
    where bx = typeToBoxLHS t
          (lhs, argTypes) = match args bx
          Operator xd vars = lhs
          --abc = lhs == _
          lhsWithName = addText (0,prettyPrint symb) lhs
          --rhsEmptyBox = typeToHole t
          (newRHS,newBindings) = eatArgs args (Hole xd (map lhsVarToExpr vars)) -- TODO: make this work with patterns
          rhsBox = addthings newRHS rhsExpr (newBindings ++ env)


addthings :: HoleBD -> Exp l -> Env HoleBD
addthings t (Paren _ body) env = addthings t body env
addthings t (HS.Lambda _ pats body) env = let (t',bindings) = (eatArgs pats t) in addthings t' body (bindings++env)
addthings t (HS.Var _ (UnQual _ (Ident _ nm))) env = let bx = getExpr nm env in simpleFill t bx
addthings t (App _ a b) env = simpleFill t (rawBox a [(\h -> addthings h b (incDepthsBindings env))] env)
addthings t (InfixApp _ l (QVarOp _ qname ) r) env = simpleFill t (rawBox (HS.Var undefined qname) [(\h -> addthings h b (incDepthsBindings env)) | b <- [l,r]] env)
--addthings t (InfixApp _ l qop r) env = error "infix"
addthings t e env = error (show $ return () <$> e)

rawBox :: Exp l -> [HoleBD -> HoleBD] ->  Env ExprBD
rawBox (Paren _ body) args env = rawBox body args env
rawBox (HS.Var _ (UnQual _ (Ident _ nm))) args env = let (BX.Symbol xd args') = getExpr nm env in
                                                         BX.Symbol xd (simpleFillHoles' args' args)
rawBox (HS.Var _ (UnQual _ (HS.Symbol _ nm))) args env = let (BX.Symbol xd args') = getExpr ("("++nm++")") env in
                                                             BX.Symbol xd (simpleFillHoles' args' args)
rawBox (App _ a b) args env = rawBox a ((\h -> addthings h b (incDepthsBindings env)):args ) env
rawBox s args env = error (show $ return () <$> s)

-- fills top-level holes recursively without doing proper checks that things work
simpleFill :: HoleBD -> ExprBD -> HoleBD
simpleFill (Hole hxd hChs) (BX.Symbol bxd aChs) = Filled hxd (BX.Symbol bxd $ simpleFillHoles aChs hChs) []
--simpleFill (Hole hxd hChs) (HS.Symbol argLab aChs) = Filled hxd [HS.Symbol argLab (simpleFillHoles aChs hChs)]

simpleFillHoles :: [HoleBD] -> [ExprBD] -> [HoleBD]
simpleFillHoles ((Filled hxd f rest):hs) as = (Filled hxd f rest):simpleFillHoles hs as
simpleFillHoles (h:hs) as = zipWith simpleFill (map incDepthsHole (h:hs)) as
simpleFillHoles [] [] = []

simpleFillHoles' :: [HoleBD] -> [HoleBD -> HoleBD] -> [HoleBD]
simpleFillHoles' hs [] = hs
simpleFillHoles' ((Filled xd arg others):hs) (a:as) =  (Filled xd arg others) : simpleFillHoles' hs (a:as)
simpleFillHoles' ((Hole xd others):hs) (a:as) = a (incDepthsHole (Hole xd others)) : simpleFillHoles' hs as

incDepthsBindings :: [(String,BindTarget)] ->[(String,BindTarget)]
incDepthsBindings ((n, Local e):rs) = (n,Local (incDepthsExpr e)):incDepthsBindings rs
incDepthsBindings (b:rs) = b:incDepthsBindings rs



-- Remove things from a hole and turn them into bindings
eatArgs :: [Pat l] -> HoleBD -> (HoleBD,[(String,BindTarget)])
eatArgs pats t = foldl' (\ (Hole lab (ch:chs), bs) (PVar _ (Ident _ s)) -> (Hole lab chs,(s,Local ch):bs) ) (t,[]) pats
-- returned bindings are in the reverse order to how they appear, but that's good since it means (\ x x -> x) behaves the same as (\ x -> (\ x -> x)), although the first is not valid haskell

{-TODO: more complicated patterns
eatArgs pats t = let (t',bindings,n) = foldl' (\ (Node lab (ch:chs), bs, n) (PVar _ (Ident _ s)) -> (,,) ) (t,[],0) pats
                 in incArgIndices (length bindings - length pats) t'-}


-- Put patterns into an LHS, return the modified LHS and the new local bindings
match :: [Pat l] -> LHSBD -> (LHSBD,[(String,BindTarget)])
match pats (Operator xd args) = (Operator xd argBoxes, typings)
    where
        addName nd (PVar _ (Ident _ s)) = (addText (0,s) nd)
        argBoxes =  zipWith (\ bx -> maybe bx (addName bx) ) args (map Just pats ++ repeat Nothing)
        typings = zipWith (\ bx (PVar _ (Ident _ s)) -> (s, Local$ lhsVarToExpr bx)) args pats
        -- unnamedArgs = drop (length pats) (map (\(Node Box{typ=Just ty} _)->ty) args)
