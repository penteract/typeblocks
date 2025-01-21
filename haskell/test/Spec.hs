import Language.Haskell.Interpreter
import Graphics.Gloss.Interface.IO.Animate
import Graphics.UI.GLUT.Fonts (stringWidth,fontHeight,StrokeFont(Roman))
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss


p = Pictures [Translate 0.0 (-0.0) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(121.90401,0.0),(121.90401,822.8521),(0.0,822.8521),(0.0,0.0)])],Translate 60.952003 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(1493.1361,0.0),(1493.1361,-761.9001),(0.0,-761.9001),(0.0,0.0)]),Translate 521.664 (-304.76) (Text "|->")],Translate 60.952003 (-182.85602) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(399.76,0.0),(399.76,-396.188),(0.0,-396.188),(0.0,0.0)]),Translate 60.952003 (-121.904) (Text "f")],Translate 160.904 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(177.904,0.0),(177.904,-274.284),(0.0,-274.284),(0.0,0.0)]),Translate 60.952003 (-60.952003) (Text "x")]])]),Translate 788.61597 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(643.5681,0.0),(643.5681,-639.99603),(0.0,-639.99603),(0.0,0.0)])],Translate 60.952003 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(521.66406,0.0),(521.66406,-518.092),(0.0,-518.092),(0.0,0.0)]),Translate 60.952003 (-182.85599) (Text "f")],Translate 160.904 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(299.808,0.0),(299.808,-396.188),(0.0,-396.188),(0.0,0.0)])],Translate 60.952003 (-60.952003) (Pictures [Pictures [Color (makeColor 0.0 0.0 0.0 1.0) (Line [(0.0,0.0),(177.904,0.0),(177.904,-274.284),(0.0,-274.284),(0.0,0.0)]),Translate 60.952003 (-60.952003) (Text "x")]])])])])])])]


main :: IO ()
main = do
  display (InWindow "Nice Window" (1920, 1080) (10, 10)) white  p

draw :: IO ()
draw = do
  print "test-start"
  animateIO (InWindow "Nice Window" (200, 200) (10, 10)) white (\ t -> do
    --print "hello-testing"
    n <-  stringWidth Roman "hello"
    --print =<< stringWidth Roman "hello"
    let top = 119.05
    let bot = -33.33
    f <- fontHeight Roman
    True <- return (abs (f+bot-top) < 0.02)
    -- print (abs (f+bot-top) < 0.01)

    return $ Pictures [Circle 80, Text "æ{<-}-xRł", Line [(0,0),(fromIntegral n,f/2 + bot)] ]) (const$ return ())


-- from https://hackage.haskell.org/package/GLUT-2.7.0.16/docs/Graphics-UI-GLUT-Fonts.html#t:StrokeFont
-- A proportionally spaced Roman Simplex font for ASCII characters 32 through 127. The maximum top character in the font is 119.05 units; the bottom descends 33.33 units.

showImports :: String -> IO ()
showImports moduleName = do
  res <- runInterpreter (do
    x <- getModuleExports moduleName
    setImports [moduleName,"Prelude"]
    ts <- mapM id [typeOf f | Fun f <- x]
    return$ zip x ts)
  case res of
    Left e -> print e
    Right x -> putStr$ (unlines. map (\ (a,b) -> show a++"::"++b++"\n")) (x)

{-

import GHC.Tc.Utils.Monad --(initTc)
import GHC.Iface.Load --(loadModuleInterface)
import GHC.Driver.Main
--import GHC.Types.SourceFile
--import  GHC.Utils.Outputable
import GHC.Unit.Types

import GHC
import GHC.Paths ( libdir )
import GHC.Driver.Session --( defaultFatalMessager, defaultFlushOut )
import Control.Monad.IO.Class
import GHC.Utils.Outputable
import GHC.SysTools.BaseDir
import GHC.SysTools



-- | Initialize HscEnv from an optional top_dir path
initHscEnv :: Maybe FilePath -> IO HscEnv
initHscEnv mb_top_dir = do
  top_dir <- findTopDir mb_top_dir
  mySettings <- initSysTools top_dir
  myLlvmConfig <- lazyInitLlvmConfig top_dir
  dflags <- initDynFlags (defaultDynFlags mySettings myLlvmConfig)
  hsc_env <- newHscEnv dflags
  --checkBrokenTablesNextToCode (hsc_logger hsc_env) dflags
  setUnsafeGlobalDynFlags dflags
   -- c.f. DynFlags.parseDynamicFlagsFull, which
   -- creates DynFlags and sets the UnsafeGlobalDynFlags
  return hsc_env


main :: IO ()
main = {- do
  putStrLn "\n\n======Hello Test\n\n"

  he <- initHscEnv (Just libdir)
  initTcInteractive he (loadSrcInterface empty (mkModuleName "Prelude") NotBoot Nothing)
  return ()-}
  do
    putStrLn "Hello Test\n\n\n"
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        putMsgM (ppr "(4::Int)")
        dflags <- getSessionDynFlags
        setSessionDynFlags dflags
        target <- guessTarget "Main.hs" Nothing
        setTargets [target]
        load LoadAllTargets
        return ()

{-
main :: IO ()
main = do
    henv <- initHscEnv
    initTc henv HsSrcFile False (mkHoleModule (mkModuleName "Main")) undefined $ do
        loadModuleInterface undefined undefined undefined
        return ()-}

    --putStrLn "Test suite not yet implemented"


-- initTc

 -- loadModuleInterface
 -}
