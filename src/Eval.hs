module Eval where
import           Types
import           Error
import qualified Data.Map                      as Map
import           Data.Function                  ( fix )

impossibleError :: String
impossibleError = "Type checker failed. This error should be impossible!"

-- It is needed to deal with looped dependencies
type DefsEvaluator = Env -> Defs -> Either InterpreterError Env

evalPartial :: DefsEvaluator -> Env -> Exp -> Val
evalPartial _ Env { fenv = rho } (EVar n) = case n `Map.lookup` rho of
  Just TVal { onlyVal = v } -> v
  _                         -> error impossibleError
evalPartial evalDefs env (ELet defs e) = case evalDefs env defs of
  Right env' -> evalPartial evalDefs env' e
  _          -> error impossibleError
evalPartial evalDefs env (EApp e1 e2) = case evalPartial evalDefs env e1 of
  VFun f -> f $ evalPartial evalDefs env e2
  _      -> error impossibleError

evalFDef :: DefsEvaluator -> FDef -> Env -> Val
evalFDef evalDefs (FDef _ e     ) env = evalPartial evalDefs env e
evalFDef evalDefs (FDefWh _ e wh) env = case evalDefs env wh of
  Right env' -> evalPartial evalDefs env' e
  _          -> error impossibleError

modifyEnvOn :: FName -> Val -> Env -> Env
modifyEnvOn name val env@Env { fenv = rhoF } =
  let tp :: Type
      tp = onlyType (rhoF Map.! name)
  in  env { fenv = Map.insert name (TVal tp val) rhoF }

getEnv :: DefsEvaluator -> [FDef] -> Env -> Env
getEnv evalDefs fdfs = fix
  (\et e -> foldr
    (\fdf e' -> modifyEnvOn (fname fdf) (evalFDef evalDefs fdf e') e')
    (et e)
    fdfs
  )
