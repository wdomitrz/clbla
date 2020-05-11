import           System.Environment
import           System.Exit
import           System.IO
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map                      as Map
import           Parser.ErrM                    ( Err(..) )
import           Parser.ParClbla                ( myLexer
                                                , pProgramme
                                                )
import           Parser.LexClbla               as Lex
import           Parser.LayoutClbla             ( resolveLayout )
import           ProcessModule                  ( evalTypeCheckDefs )
import           Error
import           ProcessFile
import           Types
import           TypeCheck
import           PostParser
import           BaseEnv

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  (no arguments)  Start interactive mode in a default environment,"
    , "  (file)          Start interactive mode in evident of this file."
    ]
  exitFailure

interactiveModeModuleName :: MName
interactiveModeModuleName = "<inteactive>"

interactiveModeExpressionName :: String
interactiveModeExpressionName = "interactiveModeExpression"

pExp :: String -> Err Exp
pExp =
  (( (\(Module _ _ _ (Defs _ _ (fdf : _))) -> fexp fdf)
   . procModule interactiveModeModuleName
   ) <$>
    )
    . pProgramme
    . resolveLayout True
    . myLexer

evalTypeOfM :: Env -> Exp -> Either InterpreterError Type
evalTypeOfM s expr = evalState
  (runExceptT (typeOfM expr))
  TCState { tps           = onlyType <$> fenv s
          , defsProcessor = evalTypeCheckDefs s
          , uniqueTId     = 1
          }

interactiveModeHelp :: IO ()
interactiveModeHelp = hPutStrLn stderr $ unlines
  [ ":h                   Show this help,"
  , ":t (exp)             Show the type of the expression exp,"
  , ":v (variable name)   Show the value of the given variable,"
  , "otherwise            Try to evaluate the given value. You can use extensions, imports, type definitions and function definitions with declarations (use ;)."
  ]

printErr :: Show a => a -> IO ()
printErr = hPrint stderr

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

toFName :: String -> Maybe FName
toFName s = foldr go Nothing $ myLexer s
 where
  go :: Lex.Token -> Maybe FName -> Maybe FName
  go _                               x@(Just _) = x
  go (PT _ (Lex.T_UIdent         n)) Nothing    = Just $ Prefix n
  go (PT _ (Lex.T_LIdent         n)) Nothing    = Just $ Prefix n
  go (PT _ (T_InfixFunctionNameA n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameB n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameC n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameD n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameE n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameF n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameG n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameH n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameI n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameJ n)) Nothing    = Just $ Infix n
  go (PT _ (T_InfixFunctionNameK n)) Nothing    = Just $ Infix n
  go _                               Nothing    = Nothing

interactiveMode :: Env -> IO ()
interactiveMode rho = getContents >>= (foldM_ go rho . lines)
 where
  go :: Env -> String -> IO Env
  go rho' (':'       : 'h' : _) = interactiveModeHelp >> return rho'
  go rho' (':' : 't' : ' ' : s) = do
    case pExp (interactiveModeExpressionName ++ " = " ++ s) of
      Ok expr -> case evalTypeOfM rho expr of
        Left  e -> printErr e
        Right t -> print t
      Bad e -> printErr e
    return rho'
  go rho' (':' : 'v' : ' ' : s) = do
    let rhoF' = fenv rho'
    case toFName s of
      Nothing -> putStrLnErr $ "Invalid name `" ++ s ++ "`."
      Just n  -> case n `Map.lookup` rhoF' of
        Nothing ->
          putStrLnErr $ "No variable `" ++ show n ++ "` in the environment."
        Just v -> print v
    return rho'
  go rho' s = do
    newRhoE' <- runExceptT
      $ processModuleInEnv rho' interactiveModeModuleName (return s)
    case newRhoE' of
      Left  e       -> printErr e >> return rho'
      Right newRho' -> return newRho'

moduleLoadingError :: InterpreterError -> IO ()
moduleLoadingError e =
  putStrLnErr
    $  "Module loading failed, starting in a default environment. The error: "
    ++ show e

runInterperter :: IOWithInterpreterError Env -> IO ()
runInterperter rhoEIO = do
  rhoE <- runExceptT rhoEIO
  rho  <- case rhoE of
    Left  e         -> moduleLoadingError e >> return (baseEnv [])
    Right rho@Env{} -> return rho
  interactiveMode rho

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> runInterperter $ return $ baseEnv []
    [fileName] -> runInterperter $ processModule fileName (readFile fileName)
    _          -> usage
