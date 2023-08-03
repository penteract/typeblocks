{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module BoxTypes where

import Control.Arrow
import Data.Char
import Data.Maybe(fromJust)
{-
import Data.Typeable
import Data.Data
import GHC.Generics-}

import Graphics.Rendering.Cairo

import Colors
import Paths

type Field = String

data DefnBox l = Defn l [(LHSBox l,HoleBox l)] | Builtin l (ExprBox l) | Lambda l (ExprBox l) deriving (Show,Functor, Foldable, Traversable)
data LHSBox l = Operator l [PatternBox l] deriving (Show,Functor,Foldable,Traversable)
data PatternBox l = Var l [LHSAntiHoleBox l] | Constructor l [PatternBox l] deriving (Show,Functor,Foldable,Traversable)
data LHSHoleBox l = LHSHole l [LHSAntiHoleBox l] deriving (Show,Functor,Foldable,Traversable)
data LHSAntiHoleBox l = LHSAntiHole l [LHSHoleBox l] deriving (Show,Functor,Foldable,Traversable)
data HoleBox l = Filled l (ExprBox l) [ExprBox l] | Hole l [ExprBox l] deriving (Show,Functor,Foldable,Traversable)
data ExprBox l = Symbol l [HoleBox l]
                 | Case l [(PatternBox l,HoleBox l)]
                 | Let l [DefnBox l] (HoleBox l)
                 | Setter l (ExprBox l) Field (ExprBox l)
                                                 deriving (Show,Functor, Foldable, Traversable)
{-
data Travarsor d l p lh lah h e = Traversor{
    defn_tr :: DefnBD -> Travarsor d l p lh lah h e -> d
    lhs_tr ::
    }
        -}
class Ann m where
    getAnn :: m a -> a
    modifyAnn :: (a->a) -> m a -> m a


instance Ann DefnBox where
    getAnn (Defn x _) = x
    getAnn (Builtin x _) = x
    getAnn (Lambda x _) = x
    modifyAnn f (Defn x b) = Defn (f x) b
    modifyAnn f (Builtin x b) = Builtin (f x) b
    modifyAnn f (Lambda x b) = Lambda (f x) b
instance Ann LHSBox where
    getAnn (Operator x _) = x
    modifyAnn f (Operator x b) = Operator (f x) b
instance Ann PatternBox where
    getAnn (Var x _) = x
    getAnn (Constructor x _) = x
    modifyAnn f (Var x b) = Var (f x) b
    modifyAnn f (Constructor x b) = Constructor (f x) b
instance Ann LHSHoleBox where
    getAnn (LHSHole x _) = x
    modifyAnn f (LHSHole x b) = LHSHole (f x) b
instance Ann LHSAntiHoleBox where
    getAnn (LHSAntiHole x _) = x
    modifyAnn f (LHSAntiHole x b) = LHSAntiHole (f x) b
instance Ann HoleBox where
    getAnn (Filled x _ _) = x
    getAnn (Hole x _) = x
    modifyAnn f (Filled x b1 b2 ) = Filled (f x) b1 b2
    modifyAnn f (Hole x b) = Hole (f x) b
instance Ann ExprBox where
    getAnn (Symbol x _) = x
    getAnn (Case x _) = x
    getAnn (Let x _ _) = x
    getAnn (Setter x _ _ _) = x
    modifyAnn f (Symbol x b) = Symbol (f x) b
    modifyAnn f (Case x b) = Case (f x) b
    modifyAnn f (Let x b1 b2) = Let (f x) b1 b2
    modifyAnn f (Setter x b1 b2 b3) = Setter (f x) b1 b2 b3

type DefnBD = DefnBox BoxData
type LHSBD = LHSBox BoxData
type PatternBD = PatternBox BoxData
type LHSHoleBD = LHSHoleBox BoxData
type LHSAntiHoleBD = LHSAntiHoleBox BoxData
type HoleBD = HoleBox BoxData
type ExprBD = ExprBox BoxData


data TextBox = TextBox String Double deriving Show

deriving instance Show TextExtents
deriving instance Show FontExtents

makeText :: String -> Render TextBox
makeText s = do
  fex <- getFontMatrix
  ex <- textExtents s
  --liftIO (print (fex))
  return$ TextBox s (textExtentsXadvance ex)

setSourceCol :: Col -> Render ()
setSourceCol (r,g,b) = setSourceRGB r g b

type TypeName = String
data Type = Base TypeName | Type :-> Type deriving (Show)

data BoxData = Box {
      texts :: [(Int, TextBox)]
    , borderCol:: Col
    , fillCol :: Col -- box col, border col
    , outerShape :: BoxShape
    , typ :: Maybe Type
    , scope :: Maybe Int -- how far up the tree to go to find parent. Nothing indicates global scope
    , argIndex :: Int -- Index within scope, always 0 if global scope.
    , isFilled :: Bool
    } deriving Show

defaultData :: BoxData
defaultData = Box {
    texts=[]
  , fillCol=white
  , borderCol=black
  , outerShape = rect
  , typ = Nothing
  , scope = Nothing
  , argIndex = 0
  , isFilled =False
}



unpack :: Type -> ([Type],TypeName)
unpack (Base s) = ([],s)
unpack (a :-> b) = first (a:) $ unpack b


typeToBoxLHS :: Type -> LHSBD
typeToBoxLHS = modifyAnn (\xd->xd{scope=Nothing}) . typeToBox Operator (typeToBox Var typeToAntihole)

numberBoxes :: Ann bx => [bx BoxData] -> [bx BoxData]
numberBoxes = zipWith setArgIndex [0..]

setArgIndex :: Ann bx => Int -> bx BoxData -> bx BoxData
setArgIndex n = modifyAnn (\bx->bx{argIndex=n})

--data ShapeType = Hole | Antihole


typeToHole :: Type -> LHSHoleBD
typeToHole = typeToBox LHSHole typeToAntihole -- LHSHole LHSAntiHole

typeToAntihole :: Type -> LHSAntiHoleBD
typeToAntihole = typeToBox LHSAntiHole typeToHole -- LHSAntiHole LHSHole


typeToBox :: Ann a => (BoxData -> [a BoxData] -> b BoxData) -> (Type -> a BoxData) -> Type -> b BoxData
typeToBox con chfn ty =let (args, base) = unpack ty in
                        con defaultData{outerShape=getShape base,typ=Just ty, scope=Just 0}
                            (numberBoxes$ map chfn args)

getShape :: TypeName -> BoxShape
getShape nm = shapes !! (ord (head nm)`mod` length shapes)

-- Functions to let you select things from
-- Mostly boilerplate; may do more in the future (such as removing text and color)

lhsVarToExpr :: PatternBD -> ExprBD
lhsVarToExpr (Var xd args) = Symbol xd (map lhsAHtoH args)
lhsVarToExpr _ = error "Not a variable"
lhsAHtoH :: LHSAntiHoleBD -> HoleBD
lhsAHtoH (LHSAntiHole xd hs) = Hole xd (map lhsHtoExpr hs)
lhsHtoExpr :: LHSHoleBD -> ExprBD
lhsHtoExpr  (LHSHole xd ahs) = Symbol xd (map lhsAHtoH ahs)

lhsToExpr :: LHSBD -> ExprBD
lhsToExpr (Operator xd args) = Symbol xd (map lhsPatToH args) --consider stripping text from holes
lhsPatToH :: PatternBD -> HoleBD
lhsPatToH (Var xd args) = Hole xd (map lhsAHtoExpr args)
lhsPatToH (Constructor xd _) = lhsHtoH (typeToHole (fromJust (typ xd)))
lhsHtoH :: LHSHoleBD -> HoleBD
lhsHtoH (LHSHole xd ahs) = Hole xd (map lhsAHtoExpr ahs)
lhsAHtoExpr :: LHSAntiHoleBD -> ExprBD
lhsAHtoExpr (LHSAntiHole xd hs) = Symbol xd (map lhsHtoH hs)

--TODO: calculate length properly
addText :: Ann bx => (Int,String) -> bx BoxData -> bx BoxData
addText (n,s) = modifyAnn (\xd -> xd{texts=(n,TextBox s (16*fromIntegral (length s))):texts xd})

mkLocalDefn :: ExprBD -> DefnBD
mkLocalDefn e = Lambda defaultData e

{-
--TODO: calculate length properly
addText :: (Int,String) -> BoxTree -> BoxTree
addText (n,s) = modifyRoot (\b -> b{texts=(n,TextBox s (16*fromIntegral (length s))):texts b})


-- I could enforce syntax rules at the type level, and use somthing like Language.Haskell.TH.Syntax.Exp or Language.Haskell.Exts.Syntax.Dec, but zippers over them would be a pain, and they would enforce the appearence of Haskell code too strongly.
-- Another option would be to use GHC.Core.Expr (using GHC.HsToCore.deSugar), which would be very helpful for making the representation amenable to evaluation, but conflicts with niceness of display, makes it further from source files, might have trouble with representing programs during the process of construction, is too tied to evaluation details and still doesn't wouldn't be exactly represented (e.g. f vs (\ x -> f x))
--defnBox = Node defaultData



unpack :: Type -> ([Type],TypeName)
unpack (Base s) = ([],s)
unpack (a :-> b) = first (a:) $ unpack b


typeToBoxLHS :: Type -> LHSBD
typeToBoxLHS ty = typeToAntihole ty

numberHoles :: [BoxTree] -> [BoxTree]
numberHoles = zipWith setArgIndex [0..]

setArgIndex :: Int -> BoxTree -> BoxTree
setArgIndex n = modifyRoot (\bx->bx{argIndex=n})

--data ShapeType = Hole | Antihole

mkLine :: BoxTree -> BoxTree -> BoxTree
mkLine l r = addText (1,"↦") (Node defaultData [l,r])
-}
