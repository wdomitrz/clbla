module ProcessFile where
import           Control.Monad.Trans.Except
import           Control.Monad.State
import           Parser.ParClbla                ( myLexer
                                                , pProgramme
                                                )
import           Parser.LayoutClbla             ( resolveLayout )
import           Parser.ErrM                    ( Err(..) )
import           PostParser                     ( procModule )
import           Types
import           Error
import           BaseEnv
import           ProcessModule

parse :: FilePath -> String -> Parser.ErrM.Err Module
parse fn = (procModule fn <$>) . pProgramme . resolveLayout True . myLexer

interpretModuleIn :: (Module -> Env) -> Module -> IOWithInterpreterError Env
interpretModuleIn inWhat m@(Module mname _ imps defs) = do
  envs <- mapM processFile imps
  case evalState (runExceptT $ go envs) (inWhat m) of
    Left  e -> throwE $ ErrorInModule mname e
    Right x -> return x
 where
  go :: [Env] -> InterpreterStateM Env
  go envs = do
    mapM_ combineEnvs envs
    typeCheckAndProcDefs defs

interpretModule :: Module -> IOWithInterpreterError Env
interpretModule = interpretModuleIn (baseEnv . mexts)

interpretModuleInEnv :: Env -> Module -> IOWithInterpreterError Env
interpretModuleInEnv rho =
  interpretModuleIn (\m -> modifyExts (mexts m ++) rho)

parseModule :: MName -> IO String -> IOWithInterpreterError Module
parseModule mName fileContentIO = ExceptT $ do
  fileContent <- fileContentIO
  case parse mName fileContent of
    Ok  prog -> return $ return prog
    Bad s    -> runExceptT $ throwE $ ParserError s

processFile :: FilePath -> IOWithInterpreterError Env
processFile fp = processModule fp $ readFile (fp ++ ".clbla")

processModuleInEnv :: Env -> MName -> IO String -> IOWithInterpreterError Env
processModuleInEnv rho mName mContentIO =
  parseModule mName mContentIO >>= interpretModuleInEnv rho

processModule :: MName -> IO String -> IOWithInterpreterError Env
processModule mName mContentIO =
  parseModule mName mContentIO >>= interpretModule
