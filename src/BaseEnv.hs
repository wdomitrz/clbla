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
buildInsNamePrefix :: String
buildInsNamePrefix = "_"
buildInFunctions :: [(String, TVal)]
buildInFunctions = [(sName, createS), (kName, createK)]
addPrefixedNames :: [(String, a)] -> [(FName, a)]
addPrefixedNames = foldr
  (\(name, x) ->
    ([(Prefix name, x), (Prefix $ buildInsNamePrefix ++ name, x)] ++)
  )
  []

addFunToEnv :: FName -> TVal -> Env -> Env
addFunToEnv name tv = modifyFEnv (Map.insert name tv)

modifyTEnv :: (TDecls -> TDecls) -> Env -> Env
modifyTEnv f rho = rho { tenv = f $ tenv rho }

modifyFEnv :: (FDefs -> FDefs) -> Env -> Env
modifyFEnv f rho = rho { fenv = f $ fenv rho }

emptyEnv :: Env
emptyEnv = Env { exts           = []
               , localTypes     = Set.empty
               , localFunctions = Set.empty
               , tenv           = Map.empty
               , fenv           = Map.empty
               }

setExtensions :: [Ext] -> Env -> Env
setExtensions exts' rho = rho { exts = exts' }

addBuildInFunctions :: Env -> Env
addBuildInFunctions = foldr
  (\(name, tval) acc -> addFunToEnv name tval . acc)
  id
  (addPrefixedNames buildInFunctions)

baseEnv :: [Ext] -> Env
baseEnv exts' = addBuildInFunctions $ setExtensions exts' emptyEnv

combineEnvs :: Env -> InterpreterStateM ()
combineEnvs Env { localFunctions = lFunctions', tenv = rhoT', fenv = rhoF' } =
  do
    rhoT <- gets tenv
    mapM_ (goT rhoT) $ Map.assocs rhoT'
    modify (modifyTEnv (Map.union rhoT'))
    modify (modifyFEnv (Map.union $ Map.restrictKeys rhoF' lFunctions'))
 where
  goT :: TDecls -> (TName, AType) -> InterpreterStateM ()
  goT rhoT (k', v') = case k' `Map.lookup` rhoT of
    Just v -> unless (v == v') (throwE $ ModuleTypesConflictingDectariation k')
    _      -> return ()
