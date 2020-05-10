module TypeCheck where
import           Types
import           Error
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )

type TEnv = Map FName Type
data TCState = TCState {tps :: TEnv,  defsProcessor :: Defs -> Either InterpreterError TEnv, uniqueTId :: Integer}
data MPS = MPS {tcs :: TCState, unfd :: Map VName Integer}
type TypeCheckerM a = InterpreterStateMParam TCState a
type MPState a = State MPS a
type Subst = Map TId Type
type TypeUnifierM a = InterpreterStateMParam Subst a

takeNewIdMP :: MPState Integer
takeNewIdMP = do
  n <- gets (uniqueTId . tcs)
  modify (\s -> s { tcs = (tcs s) { uniqueTId = n + 1 } })
  return n

takeNewIdTC :: TypeCheckerM Integer
takeNewIdTC = do
  n <- gets uniqueTId
  modify (\s -> s { uniqueTId = n + 1 })
  return n

makePolyPoly :: Type -> MPState Type
makePolyPoly (TPoly x) = do
  unfd' <- gets unfd
  case x `Map.lookup` unfd' of
    Just n  -> return $ TVar n
    Nothing -> do
      n <- takeNewIdMP
      modify (\s -> s { unfd = Map.insert x n unfd' })
      return $ TVar n
makePolyPoly t@(TVar _   ) = return t
makePolyPoly (  t1 :-> t2) = do
  t1' <- makePolyPoly t1
  t2' <- makePolyPoly t2
  return (t1' :-> t2')
makePolyPoly (TNamed name ts) = TNamed name <$> mapM makePolyPoly ts

typeOfDefsM :: Defs -> TypeCheckerM TEnv
typeOfDefsM defs = do
  rhoOrError <- ($ defs) <$> gets defsProcessor
  case rhoOrError of
    Left  e   -> throwE e
    Right rho -> return rho

typeOfM :: Exp -> TypeCheckerM Type
typeOfM (EVar name) = do
  rho <- gets tps
  case name `Map.lookup` rho of
    Just t -> do
      st <- get
      let (tp, MPS { tcs = st' }) =
            runState (makePolyPoly t) MPS { tcs = st, unfd = Map.empty }
      put st'
      return tp
    Nothing -> throwE $ TCEVariableNotInScope name

typeOfM (ELet ds e) = do
  rho' <- typeOfDefsM ds
  modify (\s -> s { tps = rho', defsProcessor = defsProcessor s })
  typeOfM e

typeOfM (EApp e1 e2) = do
  t1 <- typeOfM e1
  t2 <- typeOfM e2
  n  <- takeNewIdTC
  let t = TVar n
  case evalMgu t1 (t2 :-> t) t of
    Right t' -> return t'
    Left  e  -> (throwE :: InterpreterError -> TypeCheckerM Type) e

mapType :: (TId -> Type) -> Type -> Type
mapType _ t@(TPoly _       ) = t
mapType f (  TVar  idx     ) = f idx
mapType f (  TNamed name ts) = TNamed name $ fmap (mapType f) ts
mapType f (  t1     :->  t2) = mapType f t1 :-> mapType f t2

evalMgu :: Type -> Type -> Type -> Either InterpreterError Type
evalMgu t t' t'' =
  evalState (runExceptT $ mgu t t' >> gets (runReader $ subst t'')) Map.empty

mgu :: Type -> Type -> TypeUnifierM ()
mgu t1 t2 | t1 == t2 = return ()
mgu t1@(TVar n1) t2
  | n1 `isIn` t2 = throwE $ TCEInfiniteType t1 t2
  | otherwise = modify
    (fmap (flip runReader (Map.singleton n1 t2) . subst) . Map.insert n1 t2)
mgu t1 t2@(TVar _)                             = mgu t2 t1

mgu (TNamed n1 ts1) (TNamed n2 ts2) | n1 == n2 = do
  when (length ts1 /= length ts2) (throwE $ TCEParamsLen n1)
  mapM_ (uncurry substsMguIn) $ zip ts1 ts2
mgu (t1 :-> t2) (t1' :-> t2') =
  mapM_ (uncurry substsMguIn) [(t1, t1'), (t2, t2')]
mgu t1 t2 = throwE $ TCECannotUnify t1 t2

substsMguIn :: Type -> Type -> TypeUnifierM ()
substsMguIn t t' = do
  nt  <- gets $ runReader $ subst t
  nt' <- gets $ runReader $ subst t'
  mgu nt nt'

isIn :: TId -> Type -> Bool
isIn n (TVar n'      ) = n == n'
isIn n (t1     :-> t2) = any (isIn n) [t1, t2]
isIn n (TNamed _   ts) = any (isIn n) ts
isIn _ _               = False

subst :: Type -> Reader Subst Type
subst t@(TVar  n      ) = asks $ Map.findWithDefault t n
subst t@(TPoly _      ) = return t
subst (  TNamed n   ts) = TNamed n <$> mapM subst ts
subst (  t1     :-> t2) = do
  t1' <- subst t1
  t2' <- subst t2
  return (t1' :-> t2')
