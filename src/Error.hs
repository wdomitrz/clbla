module Error where
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Trans.Except     ( throwE )
import           Types
import Data.Map

data InterpreterError = ParserError String | Else
  deriving (Eq, Ord, Show, Read)

type InterpreterMParam e s a = ExceptT e (Reader s) a

type InterpreterM a = InterpreterMParam InterpreterError (Map Char Char) a
