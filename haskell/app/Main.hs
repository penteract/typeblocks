import qualified Data.Map as Map
import Control.Arrow

import Language.Haskell.Exts

import GlossBackend
import Types
import Parsing
import Graphics

-- handleEvent :: Event -> [(String, DefnBD)] -> [(String, DefnBD)]
-- TODO: Implement this
handleEvent e w = w

main = do
  ParseOk mod <- parseFile "test/Sample.hs"
  print mod
  (_,bxs ) <- moduleToBoxes mod Map.empty
  print (length bxs)
  --print bxs


  mapM print (map (second (fmap (map (tText.snd). texts))) bxs)
  runUI (bxs) Graphics.draw (\ e w -> do
    let w' = handleEvent e w
    w'' <- mapM (\(a,b) -> (,) a <$> calcTexts b) w'
    --let ((_,Defn x ((Line _ lhs (Filled _ f _)):_)):_) = w''
    --print "unlaid"
    --print f
    --print "laid out"
    --print (Graphics.layoutExpr Graphics.maxWidth f)
    return$ Graphics.layout w'')
  --bxs' <-  bxs

  return ()


-- Who could possibly imagine a pure function that determined how long a piece of text is?
calcTexts :: Traversable f => f BoxData -> IO (f BoxData)
calcTexts bxds = traverse (\ d ->
  (\x -> d{texts=x}) <$>
  mapM (\ (n,TextBox s _ pos) ->
    (\ l -> (n,TextBox s l pos)) <$>
    (textLength s))
    (texts d) )
  bxds
