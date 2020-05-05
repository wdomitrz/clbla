module TypeCheck where
import           Types
import           Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.Map                      as Map
import           Data.Map

type TEnv = Map FName Type
data TCState = TCState {tps :: TEnv,  defsProcessor :: Defs -> Either InterpreterError TEnv}
type TypeCheckerM a = InterpreterReaderMParam TCState a
type Subst = Map TId Type
type TypeUnifierM a = InterpreterReaderMParam Subst a

makePolyPoly :: Type -> Type
makePolyPoly = undefined

typeOfDefsM :: Defs -> TypeCheckerM TEnv
typeOfDefsM defs = do
  rhoOrError <- ($ defs) <$> asks defsProcessor
  case rhoOrError of
    Left  e   -> throwE e
    Right rho -> return rho

typeOfM :: Exp -> TypeCheckerM Type
typeOfM (EVar name) = do
  rho <- asks tps
  case name `Map.lookup` rho of
    Just t  -> return $ makePolyPoly t
    Nothing -> throwE $ TCEVariableNotInScope name

typeOfM (ELet ds e) = do
  rho' <- typeOfDefsM ds
  local (\s -> s { tps = rho', defsProcessor = defsProcessor s }) (typeOfM e)

typeOfM (EApp e1 e2) = do
  t1 <- typeOfM e1
  t2 <- typeOfM e2
  case t1 of
    t2' :-> t -> case runMgu t2 t2' of
      Right unified -> (return :: Type -> TypeCheckerM Type)
        $ mapType (\x -> Map.findWithDefault (TVar x) x unified) t
      Left e -> (throwE :: InterpreterError -> TypeCheckerM Type) e
    _ -> throwE $ TCETypeNotFunctional t1

mapType :: (TId -> Type) -> Type -> Type
mapType _ t@(TPoly _       ) = t
mapType f (  TVar  idx     ) = f idx
mapType f (  TNamed name ts) = TNamed name $ fmap (mapType f) ts
mapType f (  t1     :->  t2) = mapType f t1 :-> mapType f t2

runMgu :: Type -> Type -> Either InterpreterError Subst
runMgu = flip flip Map.empty . ((runReader . runExceptT) .) . mgu

mgu :: Type -> Type -> TypeUnifierM Subst
mgu t1 t2 | t1 == t2 = ask
mgu t1@(TVar n1) t2@(TVar _) | t1 < t2 = mguHelper n1 t2
                             | t1 > t2 = mgu t2 t1
mgu (TVar n1) t2                               = mguHelper n1 t2
mgu t1        t2@(TVar _)                      = mgu t2 t1
mgu (TNamed n1 ts1) (TNamed n2 ts2) | n1 == n2 = do
  when (length ts1 /= length ts2) (throwE TCEParamsLen)
  rho <- ask
  foldM (\rho' (t1, t2) -> local (const rho') (mgu t1 t2)) rho (zip ts1 ts2)
mgu (t1 :-> t2) (t1' :-> t2') = do
  rho1 <- mgu t1 t1'
  local (const rho1) (mgu t2 t2')
mgu t1 t2 = throwE $ TCECannotUnify t1 t2

mguHelper :: TId -> Type -> TypeUnifierM Subst
mguHelper n1 t2 = do
  rho <- ask
  case rho !? n1 of
    Just t1' -> mgu t1' t2
    Nothing  -> asks (Map.insert n1 t2)

