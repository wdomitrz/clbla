module Error where
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Types

data InterpreterError =
    ParserError String
  | ATETypeRedeclaration TName
  | ATEConstructorRedeclaration CName
  | ATEFoldRedeclaration VName
  | ATEConflictingDefinitions [VName]
  | ATEVariableNotInScope VName
  | ATETypeNameNotUnique TName
  | ATEWrongNuberOfParams TName
  | FunConflictingDeclarations FName
  | FunDefinitionWithoutDeclaration FName
  | TCE String
  | TCEVariableNotInScope FName
  | TCETypeNotFunctional Type
  | TCECannotUnify Type Type
  | TCEParamsLen
  deriving (Eq, Ord, Show, Read)

type InterpreterReaderMParam s a = ExceptT InterpreterError (Reader s) a
type InterpreterStateMParam s a = ExceptT InterpreterError (State s) a
type InterpreterReaderM a = InterpreterStateMParam Env a
type InterpreterStateM a = InterpreterStateMParam Env a

