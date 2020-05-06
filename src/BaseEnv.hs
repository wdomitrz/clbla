module BaseEnv where
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Error
import           Types
import           BaseFunctions                  ( createS
                                                , createK
                                                )

foldPrefix, elimPrefix :: String
elimPrefix = "elim"
foldPrefix = "fold"
sName, kName :: String
sName = "s"
kName = "k"

addFunToEnv :: FName -> TVal -> Env -> Env
addFunToEnv name tv = modifyFEnv (Map.insert name tv)

modifyTEnv :: (TDecls -> TDecls) -> Env -> Env
modifyTEnv f rho = rho { tenv = f $ tenv rho }

modifyFEnv :: (FDefs -> FDefs) -> Env -> Env
modifyFEnv f rho = rho { fenv = f $ fenv rho }

emptyEnv :: Env
emptyEnv =
  Env { exts = [], localNames = Set.empty, tenv = Map.empty, fenv = Map.empty }

setExtensions :: [Ext] -> Env -> Env
setExtensions exts' rho = rho { exts = exts' }

addS, addK :: Env -> Env
addS = addFunToEnv sName createS
addK = addFunToEnv kName createK

baseEnv :: [Ext] -> Env
baseEnv exts' = addS $ addK $ setExtensions exts' emptyEnv

combineEnvs :: Env -> InterpreterStateM ()
combineEnvs Env { tenv = rhoT', fenv = rhoF' } = do
  rhoT <- gets tenv
  mapM_ (goT rhoT) $ Map.assocs rhoT'
  modify (modifyTEnv (Map.union rhoT'))
  modify (modifyFEnv (Map.union rhoF'))
 where
  goT :: TDecls -> (TName, AType) -> InterpreterStateM ()
  goT rhoT (k', v') = case k' `Map.lookup` rhoT of
    Just v -> unless (v == v') (throwE $ ModuleTypesConflictingDectariation k')
    _      -> return ()
