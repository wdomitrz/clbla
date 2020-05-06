module BaseEnv where
import           Types
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           BaseFunctions                  ( createS
                                                , createK
                                                )

sName, kName :: String
sName = "s"
kName = "k"

addFunToEnv :: FName -> TVal -> Env -> Env
addFunToEnv name tv rho = rho { fenv = Map.insert name tv $ fenv rho }

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
