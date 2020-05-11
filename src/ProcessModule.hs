module ProcessModule where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           TypeCheck
import           Eval
import           BaseFunctions
import           BaseEnv
import           Exts
import           Types
import           Error


runTypeCheckDefs :: Env -> Defs -> (Either InterpreterError TEnv, Env)
runTypeCheckDefs env defs = runState (runExceptT $ typeCheckDefs defs) env

evalTypeCheckDefs :: Env -> Defs -> Either InterpreterError TEnv
evalTypeCheckDefs = (fst .) . runTypeCheckDefs

evalDefs :: Env -> Defs -> Either InterpreterError Env
evalDefs env defs = evalState (runExceptT $ procDefs defs) env

procDefsInitial :: Defs -> InterpreterStateM ()
procDefsInitial (Defs tdfs fdcls _) = do
  resetLocalNames
  mapM_ procTDecl tdfs
  mapM_ procTDef  tdfs
  mapM_ procFDecl fdcls

areFDefsAllUnique :: [FDef] -> InterpreterStateM ()
areFDefsAllUnique = foldM_ go [] . fmap fname
 where
  go :: [FName] -> FName -> InterpreterStateM [FName]
  go fns fn = when (fn `elem` fns) (throwE $ FunConflictingDefinitions fn)
    >> return (fn : fns)

typeCheckDefs :: Defs -> InterpreterStateM TEnv
typeCheckDefs dfs@(Defs _ _ fdfs) = do
  procDefsInitial dfs
  areFDefsAllUnique fdfs
  mapM_ typeCheckFDef fdfs
  gets (fmap onlyType . fenv)

procDefs :: Defs -> InterpreterStateM Env
procDefs dfs@(Defs _ _ fdfs) = procDefsInitial dfs >> procFDefs fdfs

typeCheckAndProcDefs :: Defs -> InterpreterStateM Env
typeCheckAndProcDefs dfs@(Defs _ _ fdfs) = do
  procDefsInitial dfs
  areFDefsAllUnique fdfs
  mapM_ typeCheckFDef fdfs
  procFDefs fdfs

procFDefs :: [FDef] -> InterpreterStateM Env
procFDefs fdfs = do
  rho <- get
  let rhoF = fenv rho
  let go :: FDef -> (FName, (Type, FDef))
      go fdf =
        let name = fname fdf in (name, (onlyType (rhoF Map.! name), fdf))
  return $ getEnvWith evalDefs (Map.fromList (fmap go fdfs)) rho

typeCheckFDefInitial :: FName -> InterpreterStateM Type
typeCheckFDefInitial name = do
  rhoF <- gets fenv
  vt   <- case name `Map.lookup` rhoF of
    Nothing -> throwE $ FunDefinitionWithoutDeclaration name
    Just t  -> return t
  case vt of
    -- function declared and defined before, but not in this scope
    TVal _ _ -> throwE $ FunDefinitionWithoutDeclaration name
    NoVal t  -> return t

typeCheckFDefFinalize :: Exp -> Type -> TCState -> InterpreterStateM ()
typeCheckFDefFinalize exp' t tcsState = do
  t' <- case evalState (runExceptT (typeOfM exp')) tcsState of
    Left  e  -> throwE e
    Right t' -> return t'
  case evalMgu t t' t' of
    Left  e -> throwE e
    Right _ -> return ()

typeCheckFDef :: FDef -> InterpreterStateM ()
typeCheckFDef (FDef name exp') = do
  t <- typeCheckFDefInitial name
  s <- get
  typeCheckFDefFinalize
    exp'
    t
    TCState { tps           = onlyType <$> fenv s
            , defsProcessor = evalTypeCheckDefs s
            , uniqueTId     = 1
            }

typeCheckFDef (FDefWh name exp' wh) = do
  t  <- typeCheckFDefInitial name
  s' <- get
  let (rhoTCOrError, s) = runTypeCheckDefs s' wh
  rhoTC <- case rhoTCOrError of
    Left  e     -> throwE e
    Right rhoTC -> return rhoTC
  typeCheckFDefFinalize
    exp'
    t
    TCState { tps = rhoTC, defsProcessor = evalTypeCheckDefs s, uniqueTId = 0 }

procFDecl :: FDecl -> InterpreterStateM ()
procFDecl (FDecl name tp) = addFun name $ NoVal tp

procTDecl :: TDef -> InterpreterStateM ()
procTDecl (TDef name vs cs) = do
  civpu vs
  addType name AType { numOfParams = length vs, params = vs, constrs = cs }

-- Check variables in scope
cvis :: [VName] -> Type -> InterpreterStateM ()
cvis vs t = case t of
  TPoly n            -> unless (n `elem` vs) $ throwE $ ATEVariableNotInScope n
  TVar  _            -> pure ()
  TNamed _        ts -> mapM_ (cvis vs) ts
  (      t1 :-> t2)  -> cvis vs t1 >> cvis vs t2

procTDef :: TDef -> InterpreterStateM ()
procTDef t@(TDef name vs cs) = do
  -- Check if elements of vs are unique is in procTDecl.
  -- Check if all type variables are in scope
  mapM_ (\(TConstr _ ts) -> mapM_ (cvis vs) ts) cs
  mapM_ (addConstructor $ algTType t)           cs
  gets (enabled elimOff)
    >>= flip unless (addFun (Prefix (elimPrefix ++ name)) $ createElim t)
  gets (enabled foldOn)
    >>= flip when (addFun (Prefix (foldPrefix ++ name)) $ createFold t)

addConstructor :: Type -> TConstr -> InterpreterStateM ()
addConstructor td c@(TConstr cname _) = addFun cname $ createContructor td c

checkIfTypeNameIsUniqueGlobal :: TName -> InterpreterStateM ()
checkIfTypeNameIsUniqueGlobal n = do
  rhoT <- gets tenv
  when (n `Map.member` rhoT) (throwE $ ATETypeRedeclaration n)

-- Check if variable parameters are unique
civpu :: [VName] -> InterpreterStateM ()
civpu = foldM_ go []
 where
  go :: [VName] -> VName -> InterpreterStateM [VName]
  go vs' v' = when (v' `elem` vs') (throwE $ ATEConflictingDefinitions v')
    >> return (v' : vs')


addType :: TName -> AType -> InterpreterStateM ()
addType name t = do
  -- Check uniques of Type name
  checkIfTypeNameIsUniqueGlobal name
  modify (\rho -> rho { localTypes = name `Set.insert` localTypes rho })
  modify (\rho -> rho { tenv = Map.insert name t $ tenv rho })

checkIfFuctionNameIsUniqueLocal :: FName -> InterpreterStateM ()
checkIfFuctionNameIsUniqueLocal n =
  gets localFunctions
    >>= (flip when (throwE $ FunRedeclaration n) . (n `Set.member`))

-- Check types in scope and correct number of params
ctisacnop :: Type -> InterpreterStateM ()
ctisacnop t = case t of
  TPoly _     -> pure ()
  TVar  _     -> pure ()
  TNamed n ts -> do
    rhoT <- gets tenv
    unless (n `Map.member` rhoT) (throwE $ ATEVariableNotInScope n)
    unless (numOfParams (rhoT Map.! n) == length ts)
           (throwE $ ATEWrongNuberOfParams n)
    mapM_ ctisacnop ts
  (t1 :-> t2) -> ctisacnop t1 >> ctisacnop t2

addFun :: FName -> TVal -> InterpreterStateM ()
addFun name f = do
  ctisacnop $ onlyType f
  checkIfFuctionNameIsUniqueLocal name
  modify (\rho -> rho { localFunctions = name `Set.insert` localFunctions rho })
  modify (addFunToEnv name f)
