module Eval where
import           Types
import           Error
import qualified Data.Map                      as Map
import           Data.Function                  ( fix )

impossibleError :: String
impossibleError = "Type checker failed. This error should be impossible!"

-- It is needed to deal with looped dependencies
type DefsEvaluator = Env -> Defs -> Either InterpreterError Env

evalPartial :: DefsEvaluator -> Exp -> Env -> Val
evalPartial _ (EVar n) Env { fenv = rho } = case n `Map.lookup` rho of
  Just TVal { onlyVal = v } -> v
  _                         -> error impossibleError
evalPartial evalDefs (ELet defs e) env = case evalDefs env defs of
  Right env' -> evalPartial evalDefs e env'
  Left  _    -> error impossibleError
evalPartial evalDefs (EApp e1 e2) env = case evalPartial evalDefs e1 env of
  VFun f -> f $ evalPartial evalDefs e2 env
  _      -> error impossibleError
