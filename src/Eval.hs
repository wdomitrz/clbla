module Eval where
import           Types
import           Error
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           BaseFunctions

impossibleError :: String
impossibleError = "Type checker failed. This error should be impossible!"

-- It is needed to deal with looped dependencies
type DefsEvaluator = Env -> Defs -> Either InterpreterError Env

evalPartial :: DefsEvaluator -> Env -> Exp -> Val
evalPartial _ Env { fenv = rho } (EVar n) = onlyVal $ rho Map.! n
evalPartial evalDefs env (ELet defs e) = case evalDefs env defs of
  Right env' -> evalPartial evalDefs env' e
  _          -> error impossibleError
evalPartial evalDefs env (EApp e1 e2) =
  unVFun (evalPartial evalDefs env e1) $ evalPartial evalDefs env e2
evalFDef :: DefsEvaluator -> Env -> (Type, FDef) -> TVal
evalFDef evalDefs env (tp, (FDef _ expr)) =
  TVal tp $ evalPartial evalDefs env expr
evalFDef evalDefs env (tp, (FDefWh _ expr wh)) = case evalDefs env wh of
  Right env' -> TVal tp $ evalPartial evalDefs env' expr
  _          -> error impossibleError

getEnvWith :: DefsEvaluator -> Map FName (Type, FDef) -> Env -> Env
getEnvWith evalDefs fdfs env = env { fenv = rhoFNew }
 where
  rhoF, rhoFNew :: FDefs
  rhoF = fenv env
  rhoFNew =
    let rhoF' = Map.union
          (fmap (\e -> evalFDef evalDefs env { fenv = rhoF' } e) fdfs)
          rhoF
    in  rhoF'
