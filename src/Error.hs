module Error where
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Types

data InterpreterError =
    ParserError String
  | ATETypeRedeclaration TName
  | ATEConstructorRedeclaration CName
  | ATEElimRedeclaration VName
  | FunRedeclaration FName
  | ATEConflictingDefinitions VName
  | ATEVariableNotInScope VName
  | ATETypeNameNotUnique TName
  | ATEWrongNuberOfParams TName
  | FunConflictingDeclarations FName
  | FunDefinitionWithoutDeclaration FName
  | ModuleTypesConflictingDectariation TName
  | ErrorInModule MName InterpreterError
  | TCE String
  | TCEInfiniteType Type Type
  | TCEVariableNotInScope FName
  | TCETypeNotFunctional Type
  | TCECannotUnify Type Type
  | TCEParamsLen
  deriving (Eq, Ord, Show, Read)

type InterpreterReaderMParam s a = ExceptT InterpreterError (Reader s) a
type InterpreterStateMParam s a = ExceptT InterpreterError (State s) a
type InterpreterReaderM a = InterpreterReaderMParam Env a
type InterpreterStateM a = InterpreterStateMParam Env a

type IOWithInterpreterError = ExceptT InterpreterError IO

