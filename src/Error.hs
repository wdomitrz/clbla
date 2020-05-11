module Error where
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           Types

data InterpreterError =
    ParserError String
  | ATETypeRedeclaration TName
  | FunRedeclaration FName
  | ATEConflictingDefinitions VName
  | ATEVariableNotInScope VName
  | ATETypeNameNotUnique TName
  | ATEWrongNuberOfParams TName
  | FunConflictingDeclarations FName
  | FunDefinitionWithoutDeclaration FName
  | FunConflictingDefinitions FName
  | ModuleTypesConflictingDectariation TName
  | ErrorInModule MName InterpreterError
  | TCEInfiniteType Type Type
  | TCEVariableNotInScope FName
  | TCETypeNotFunctional Type
  | TCECannotUnify Type Type
  | TCEParamsLen TName
  deriving (Eq, Ord, Read)
instance Show InterpreterError where
  show (ParserError s) = "Error while parsing: " ++ s
  show (ATETypeRedeclaration t) = "Redeclaraion of a type: `" ++ t ++ "`."
  show (FunRedeclaration f) = "Redeclaraion of a function `" ++ show f ++ "`."
  show (ATEConflictingDefinitions v) =
    "Conflicting declarations of a variable `" ++ v ++ "`."
  show (ATEVariableNotInScope v) = "Variable not in scope `" ++ v ++ "`."
  show (ATETypeNameNotUnique t) =
    "The name of `" ++ show t ++ "` type is not unique."
  show (ATEWrongNuberOfParams t) =
    "Wron number of parameters for the type `" ++ t ++ "`."
  show (FunConflictingDeclarations f) =
    "Conflicting declarations of a function `" ++ show f ++ "`."
  show (FunDefinitionWithoutDeclaration f) =
    "Function `" ++ show f ++ "` defined, but not declared."
  show (FunConflictingDefinitions f) =
    "Conflicting definitions of function `" ++ show f ++ "`."
  show (ModuleTypesConflictingDectariation t) =
    "Conflicting declarations of type `" ++ t ++ "` in imported modules."
  show (ErrorInModule m e) = "Error in module `" ++ m ++ "`:\n" ++ show e
  show (TCEInfiniteType t1 t2) =
    "Cannot constuct an infinite type `"
      ++ show t1
      ++ "` ~ `"
      ++ show t2
      ++ "`."
  show (TCEVariableNotInScope f) = "Variable `" ++ show f ++ "` not in scope."
  show (TCETypeNotFunctional  t) = "Type `" ++ show t ++ "` is not function."
  show (TCECannotUnify t1 t2) =
    "Cannot unify type `" ++ show t1 ++ "` with type `" ++ show t2 ++ "`."
  show (TCEParamsLen n) = "Wrong number of parameters for type`" ++ n ++ "`."

type InterpreterReaderMParam s a = ExceptT InterpreterError (Reader s) a
type InterpreterStateMParam s a = ExceptT InterpreterError (State s) a
type InterpreterReaderM a = InterpreterReaderMParam Env a
type InterpreterStateM a = InterpreterStateMParam Env a

type IOWithInterpreterError = ExceptT InterpreterError IO

