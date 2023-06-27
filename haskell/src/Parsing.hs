module Parsing where
import Data.Tree
import BoxTree
import Language.Haskell.Exts.Syntax hiding (Type)
import qualified Language.Haskell.Exts.Syntax
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


-- Not the state monad, because when we add things, we usually only want to apply it to subparts and don't want to mutate it when returning
type Env a = [(String,BoxTree)] -> a

type ModCache = Map.Map String [(String,BoxTree)]

moduleToBoxes :: Language.Haskell.Exts.Syntax.Module l -> ModCache -> IO (ModCache,[(String,BoxTree)])
moduleToBoxes (Module _ _ _ imps ds) knownModules = do
    (c', env)<- getImps (preludeDecl:imps) knownModules []
    return (c', dsToBoxes ds env)

preludeDecl = ImportDecl undefined (ModuleName undefined "Prelude") undefined undefined undefined undefined undefined undefined

getImps :: [ImportDecl l]-> ModCache -> Env (IO (ModCache,[(String,BoxTree)]))
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

dsToBoxes :: [Decl l] -> Env [(String,BoxTree)]
dsToBoxes ds env = [(name, defnToBox (typ.rootLabel =<< lookup name typeSigs) clauses (typeSigs ++ env)) | (name,clauses) <- defns]
    where
        typeSigs = [(prettyPrint n, addText (0,prettyPrint n) $ typeToAntihole (toType t)) | (TypeSig _ ns t) <- ds, n<-ns]
        defns = [ (getName (head ms), ms) | FunBind _ ms <- ds]


getName :: Match l -> String
getName (Match _ nm _ _ _) = prettyPrint nm
getName (InfixMatch _ _ nm _ _ _) = prettyPrint nm

-- Adding polymorphism will be fun
toType :: Language.Haskell.Exts.Syntax.Type l -> Type
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

defnToBox :: Maybe Type -> [Match l] -> Env BoxTree
defnToBox t lines = defnBox <$> (mapM (lineToBox t) lines)

lineToBox :: Maybe Type -> Match l -> Env BoxTree
lineToBox (Just t) (InfixMatch _ l symb rs rhs w) env = undefined
lineToBox (Just t) (Match _ symb args (UnGuardedRhs _ rhsExpr) w) env = mkLine lhsWithName rhsBox
    where bx = typeToBoxLHS t
          (lhs, argTypes) = match args bx
          lhsWithName = addText (0,prettyPrint symb) lhs
          --rhsEmptyBox = typeToHole t
          (newRHS,newBindings) = eatArgs args lhs
          rhsBox = addthings newRHS rhsExpr (newBindings ++ env)


addthings :: BoxTree -> Exp l -> Env BoxTree
addthings t (Paren _ body) env = addthings t body env
addthings t (Lambda _ pats body) env = let (t',bindings) = (eatArgs pats t) in addthings t' body (bindings++env)
addthings t (Var _ (UnQual _ (Ident _ nm))) env = let (Just bx) = lookup nm env in simpleFill t bx
addthings t (App _ a b) env = simpleFill t (rawBox a [(\h -> addthings h b (map (second incDepths) env))] env)
addthings t (InfixApp _ l (QVarOp _ qname ) r) env = simpleFill t (rawBox (Var undefined qname) [(\h -> addthings h b (map (second incDepths) env)) | b <- [l,r]] env)
--addthings t (InfixApp _ l qop r) env = error "infix"
addthings t e env = error (show $ return () <$> e)

rawBox :: Exp l -> [BoxTree -> BoxTree] ->  Env BoxTree
rawBox (Paren _ body) args env = rawBox body args env
rawBox (Var _ (UnQual _ (Ident _ nm))) args env = let (Just bx) = lookup nm env in bx{subForest=simpleFillHoles' (subForest bx) args}
rawBox (Var _ (UnQual _ (Symbol _ nm))) args env = let (Just bx) = lookup ("("++nm++")") env in bx{subForest=simpleFillHoles' (subForest bx) args}
rawBox (App _ a b) args env = (rawBox a ((\h -> addthings h b (map (second incDepths) env)):args ) env)
rawBox s args env = error (show $ return () <$> s)

simpleFill :: BoxTree -> BoxTree -> BoxTree
simpleFill (Node holeLab hChs) (Node argLab aChs) = Node holeLab{isFilled=True} [Node argLab (simpleFillHoles aChs hChs)]

simpleFillHoles :: [BoxTree] -> [BoxTree] -> [BoxTree]
simpleFillHoles (h:hs) as = if isFilled (rootLabel h) then h:simpleFillHoles hs as else zipWith simpleFill (map incDepths (h:hs)) as
simpleFillHoles [] [] = []

simpleFillHoles' :: [BoxTree] -> [BoxTree -> BoxTree] -> [BoxTree]
simpleFillHoles' (hs) [] = hs
simpleFillHoles' (h:hs) (a:as) = if isFilled (rootLabel h) then h:simpleFillHoles' hs (a:as) else a (incDepths h) : simpleFillHoles' hs as

incDepths :: BoxTree -> BoxTree
incDepths = incDepthsN 0
    where incDepthsN :: Int -> BoxTree -> BoxTree
          incDepthsN n (Node lab chs) = let newScope= fmap (\sc -> if sc<=n then sc+1 else sc) (scope lab)
                                        in Node lab{scope = newScope} (map (incDepthsN (n+1)) chs)

-- implicts = [PVar undefined . Ident undefined $ "imp_"++show i | i<-[0..]]


eatArgs :: [Pat l] -> BoxTree -> (BoxTree,[(String,BoxTree)])
eatArgs pats t = foldl' (\ (Node lab (ch:chs), bs) (PVar _ (Ident _ s)) -> (Node lab chs,(s,ch):bs) ) (t,[]) pats
-- returned bindings are in the reverse order to how they appear, but that's good since it means (\ x x -> x) behaves the same as (\ x -> (\ x -> x)), although the first is not valid haskell

{-TODO: more complicated patterns
eatArgs pats t = let (t',bindings,n) = foldl' (\ (Node lab (ch:chs), bs, n) (PVar _ (Ident _ s)) -> (,,) ) (t,[],0) pats
                 in incArgIndices (length bindings - length pats) t'-}

match :: [Pat l] -> BoxTree -> (BoxTree,[(String,BoxTree)])
match pats (Node b args) = (Node b argBoxes, typings)
    where
        addName nd (PVar _ (Ident _ s)) = (addText (0,s) nd)
        argBoxes =  zipWith (\ bx -> maybe bx (addName bx) ) args (map Just pats ++ repeat Nothing)
        typings = zipWith (\ bx (PVar _ (Ident _ s)) -> (s,unLHS bx)) args pats
        -- unnamedArgs = drop (length pats) (map (\(Node Box{typ=Just ty} _)->ty) args)

unLHS :: BoxTree -> BoxTree
unLHS bx = bx -- undo formatting due to being an LHS (make holes grey

--typesigs xs = [ | TypeSig _  <- xs]
