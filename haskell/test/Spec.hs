import Language.Haskell.Interpreter
import Graphics.Gloss.Interface.IO.Animate
import Graphics.UI.GLUT.Fonts (stringWidth,fontHeight,StrokeFont(Roman))

main :: IO ()
main = do
  draw

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
