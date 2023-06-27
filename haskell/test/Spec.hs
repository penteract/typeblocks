import Language.Haskell.Interpreter


main = do
  Right x <- runInterpreter (getModuleExports "Data.List")
  putStr$ (unlines. map show) x

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
