module ProcessModule where
import           Types
import           Error
import           Utils
import           Control.Monad                  ( mapM_ )
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           TypeCheck
import           Eval

foldPrefix, elimPrefix :: String
elimPrefix = "elim"
foldPrefix = "fold"

polyParamsNames :: [String]
polyParamsNames = liftM2 (flip (:) . show) [1 ..] ['a' .. 'z']
getFreeParamName :: [String] -> String
getFreeParamName = head . (polyParamsNames \\)

checkIfNameIsUniqueGlobal
  :: (VName -> InterpreterError) -> VName -> InterpreterStateM ()
checkIfNameIsUniqueGlobal eConstr n = do
  rhoT <- gets tenv
  rhoF <- gets fenv
  when (n `Map.member` rhoT || n `Map.member` rhoF) (throwE $ eConstr n)

checkIfNameIsUniqueLocal
  :: (VName -> InterpreterError) -> VName -> InterpreterStateM ()
checkIfNameIsUniqueLocal eConstr n = do
  lNames <- gets localNames
  when (n `Set.member` lNames) (throwE $ eConstr n)

resetLocalNames :: InterpreterStateM ()
resetLocalNames = modify (\env -> env { localNames = Set.empty })

runTypeCheckDefs :: Env -> Defs -> (Either InterpreterError TEnv, Env)
runTypeCheckDefs env defs = runState (runExceptT $ typeCheckDefs defs) env

evalTypeCheckDefs :: Env -> Defs -> Either InterpreterError TEnv
evalTypeCheckDefs = (fst .) . runTypeCheckDefs

evalDefs :: Env -> Defs -> Either InterpreterError Env
evalDefs env defs = evalState (runExceptT $ procDefs defs) env

typeCheckDefs :: Defs -> InterpreterStateM TEnv
typeCheckDefs (Defs tdfs fdcls fdfs) = do
  resetLocalNames
  mapM_ procTDecl tdfs
  mapM_ procTDef  tdfs
  -- eliminators and folds can be oveloaded
  resetLocalNames
  mapM_ procFDecl     fdcls
  mapM_ typeCheckFDef fdfs
  gets (fmap onlyType . fenv)

procDefs :: Defs -> InterpreterStateM Env
procDefs dfs@(Defs _ _ fdfs) = do
  void $ typeCheckDefs dfs
  procFDefs (Map.fromList $ fmap (\df -> (fname df, df)) fdfs)

procFDefs :: RawFDefs -> InterpreterStateM Env
procFDefs = undefined

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
typeCheckFDefFinalize exp t tcsState = do
  t' <- case runReader (runExceptT (typeOfM exp)) tcsState of
    Left  e  -> throwE e
    Right t' -> return t'
  case runMgu t t' of
    Left  e -> throwE e
    Right _ -> return ()

typeCheckFDef :: FDef -> InterpreterStateM ()
typeCheckFDef (FDef name exp) = do
  t     <- typeCheckFDefInitial name
  state <- get
  typeCheckFDefFinalize
    exp
    t
    TCState { tps           = onlyType <$> fenv state
            , defsProcessor = evalTypeCheckDefs state
            }

typeCheckFDef (FDefWh name exp wh) = do
  t      <- typeCheckFDefInitial name
  state' <- get
  let (rhoTCOrError, state) = runTypeCheckDefs state' wh
  rhoTC <- case rhoTCOrError of
    Left  e     -> throwE e
    Right state -> return state
  typeCheckFDefFinalize
    exp
    t
    TCState { tps = rhoTC, defsProcessor = evalTypeCheckDefs state }


procFDecl :: FDecl -> InterpreterStateM ()
procFDecl (FDecl name tp) = do
  checkIfNameIsUniqueLocal FunConflictingDeclarations name
  ctisacnop tp
  addFun name $ NoVal tp

procTDecl :: TDef -> InterpreterStateM ()
procTDecl (TDef name vs _) = do
  -- Check uniques of Type name
  checkIfNameIsUniqueGlobal ATETypeRedeclaration name
  tenv <- gets tenv
  unless (allUnique vs) $ throwE $ ATEConflictingDefinitions vs
  addType name (AType $ length vs)

-- Check types in scope and correct number of params
ctisacnop :: Type -> InterpreterStateM ()
ctisacnop t = case t of
  TPoly n     -> pure ()
  TVar  _     -> pure ()
  TNamed n ts -> do
    rhoT <- gets tenv
    unless (n `Map.member` rhoT) (throwE $ ATEVariableNotInScope n)
    unless (numOfParams (rhoT Map.! n) == length ts)
           (throwE $ ATEWrongNuberOfParams n)
    mapM_ ctisacnop ts
  (t1 :-> t2) -> ctisacnop t1 >> ctisacnop t2

-- Check variables in scope
cvis :: [VName] -> Type -> InterpreterStateM ()
cvis vs t = case t of
  TPoly n            -> unless (n `elem` vs) $ throwE $ ATEVariableNotInScope n
  TVar  _            -> pure ()
  TNamed n        ts -> mapM_ (cvis vs) ts
  (      t1 :-> t2)  -> cvis vs t1 >> cvis vs t2

procTDef :: TDef -> InterpreterStateM ()
procTDef t@(TDef name vs cs) = do
  -- Check if elements of vs are unique is in procTDecl.
  -- Check if all type variables are in scope and if the number of parameters to
  -- the other types is correct.
  mapM_ (\(TConstr _ ts) -> mapM_ (cvis vs) ts) cs
  mapM_ (\(TConstr _ ts) -> mapM_ ctisacnop ts) cs
  -- Check uniques of fold name
  checkIfNameIsUniqueGlobal ATEFoldRedeclaration (elimPrefix ++ name)
  addFun (elimPrefix ++ name) $ createElim t
  -- addFun (foldPrefix ++ name) $ createFold t
  mapM_ (addConstructor $ algTType t) cs

createElim, createFold :: TDef -> TVal
createElim td@(TDef name vs cs) = TVal tp val
 where
  resTp :: Type
  resTp = TPoly $ getFreeParamName vs
  tp :: Type
  tp = foldr (:->)
             (algTType td :-> resTp)
             (fmap (\(TConstr _ ts) -> foldr (:->) resTp ts) cs)
  val :: Val
  val = foldr go (VFun . getElem) cs Map.empty
  getElem :: Map CName Val -> Val -> Val
  getElem ms (VNamed cname vs) = foldl go' (ms Map.! cname) vs
  getElem _ _ =
    error
      "Type check failed - last param to the eliminator has to be an instance of a corresponding type."
  go' :: Val -> Val -> Val
  go' (VFun f) x = f x
  go' _        _ = error
    "Type failed - non-last parameter to the eliminator has to be a function"
  go :: TConstr -> (Map CName Val -> Val) -> Map CName Val -> Val
  go (TConstr cname ts) acc ms = VFun (\v -> acc (Map.insert cname v ms))
createFold = undefined

addConstructor :: Type -> TConstr -> InterpreterStateM ()
addConstructor td c@(TConstr cname _) = do
  -- Check uniques of Constructor name
  checkIfNameIsUniqueGlobal ATEConstructorRedeclaration cname
  addFun cname $ createContructor td c
createContructor :: Type -> TConstr -> TVal
createContructor baseTp (TConstr cname ts) = TVal tp val
 where
  tp :: Type
  tp = foldr (:->) baseTp ts
  val :: Val
  val = foldr go (VNamed cname) ts []
  go :: a -> ([Val] -> Val) -> [Val] -> Val
  go _ acc vs = VFun (\v -> acc (v : vs))

algTType :: TDef -> Type
algTType (TDef name vs _) = TNamed name (fmap TPoly vs)

addType :: TName -> AType -> InterpreterStateM ()
addType name t = modify (\rho -> rho { tenv = Map.insert name t $ tenv rho })

addFun :: FName -> TVal -> InterpreterStateM ()
addFun name f = do
  modify (\rho -> rho { localNames = name `Set.insert` localNames rho })
  modify (\rho -> rho { fenv = Map.insert name f $ fenv rho })
