{-# LANGUAGE StandaloneDeriving #-}
module BoxTree where
import Data.Tree
import Data.Tree.Zipper hiding (first)
import Control.Arrow
import Data.Char

import Graphics.Rendering.Cairo

import Colors
import Paths


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

--TODO: calculate length properly
addText :: (Int,String) -> BoxTree -> BoxTree
addText (n,s) = modifyRoot (\b -> b{texts=(n,TextBox s (16*fromIntegral (length s))):texts b})


-- I could enforce syntax rules at the type level, and use somthing like Language.Haskell.TH.Syntax.Exp or Language.Haskell.Exts.Syntax.Dec, but zippers over them would be a pain, and they would enforce the appearence of Haskell code too strongly.
-- Another option would be to use GHC.Core.Expr (using GHC.HsToCore.deSugar), which would be very helpful for making the representation amenable to evaluation, but conflicts with niceness of display, makes it further from source files, might have trouble with representing programs during the process of construction, is too tied to evaluation details and still doesn't wouldn't be exactly represented (e.g. f vs (\ x -> f x))
type BoxTree = Tree BoxData
modifyRoot :: (a->a) -> Tree a -> Tree a
modifyRoot fn (Node lab rs) = Node (fn lab) rs

defnBox = Node defaultData


type TypeName = String
data Type = Base TypeName | Type :-> Type deriving (Show)

unpack :: Type -> ([Type],TypeName)
unpack (Base s) = ([],s)
unpack (a :-> b) = first (a:) $ unpack b


typeToBoxLHS :: Type -> BoxTree
typeToBoxLHS ty = typeToAntihole ty

numberHoles :: [BoxTree] -> [BoxTree]
numberHoles = zipWith setArgIndex [0..]

setArgIndex :: Int -> BoxTree -> BoxTree
setArgIndex n = modifyRoot (\bx->bx{argIndex=n})

--data ShapeType = Hole | Antihole

typeToHole :: Type -> BoxTree
typeToHole = typeToBox typeToAntihole

typeToAntihole :: Type -> BoxTree
typeToAntihole = typeToBox typeToHole

typeToBox :: (Type -> BoxTree) -> Type -> BoxTree
typeToBox chfn ty = let (args, base) = unpack ty in
                        Node defaultData{outerShape=getShape base,typ=Just ty, scope=Just 0} (numberHoles$ map chfn args)


getShape :: TypeName -> BoxShape
getShape nm = shapes !! (ord (head nm)`mod` length shapes)

mkLine :: BoxTree -> BoxTree -> BoxTree
mkLine l r = addText (1,"↦") (Node defaultData [l,r])
