module Pathetic where
import           Control.Monad.Except
import           Types
import           Error

data PatheticData
type PatheticReader a = InterpreterReaderMParam PatheticData a

lambdaStar :: Defs -> Except InterpreterError Defs
lambdaStar defs@Defs { defsFDef = rhoF } = do
  rhoF' <- mapM lambdaStarFDef rhoF
  return defs { defsFDef = rhoF' }

lambdaStarFDef :: FDef -> Except InterpreterError FDef
lambdaStarFDef fdef = do
  res <- lambdaStarExp $ fexp fdef
  fdef { fexp = lambdaStarExp $ fexp fdef }

lambdaStarExp :: Exp -> Except InterpreterError Exp
lambdaStarExp = undefined

getAnnotatedTree :: Exp
getAnnotatedTree = undefined
