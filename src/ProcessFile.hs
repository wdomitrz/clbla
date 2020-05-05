module ProcessFile where
import           Parser.ParClbla                ( myLexer
                                                , pProgramme
                                                )
import           Parser.LayoutClbla             ( resolveLayout )
import           Parser.ErrM                    ( Err(..) )
import           PostParser                     ( procModule )
import           Types
import           Error
import           Control.Monad.Trans.Except
import           System.Environment

parse :: FilePath -> String -> Parser.ErrM.Err Module
parse fn = (procModule fn <$>) . pProgramme . resolveLayout True . myLexer

interpretModule :: Module -> IO (InterpreterM Env)
interpretModule = undefined

parseFile :: FilePath -> IO (InterpreterM Module)
parseFile = parseModule <*> readFile

parseModule :: FilePath -> IO String -> IO (InterpreterM Module)
parseModule fileName fileContentIO = do
  fileContent <- fileContentIO
  return $ case parse fileName fileContent of
    Ok  prog -> return prog
    Bad s    -> throwE $ ParserError s
