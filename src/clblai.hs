import           System.Environment
import           Control.Monad.Except
import           Parser.ErrM                    ( Err(..) )
import           Parser.ParClbla                ( myLexer
                                                , pProgramme
                                                )
import           Parser.LayoutClbla             ( resolveLayout )
import           Control.Monad.State
import           System.Exit
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

interactiveModeHelp :: String
interactiveModeHelp = unlines
  [ ":h                   Show this help,"
  , ":t (exp)             Show the type of the expression exp,"
  , ":v (variable name)   Show the value of the given variable,"
  , "otherwise            Try to evaluate the given value. You can use extensions, imports, type definitions and function definitions with declarations (use ;)."
  ]

interactiveMode :: Env -> IO ()
interactiveMode rho = interact (unlines . fmap go . lines)
 where
  go :: String -> String
  go (':' : 'h' : _) = interactiveModeHelp
  go (':' : 't' : ' ' : s) =
    case pExp (interactiveModeExpressionName ++ " = " ++ s) of
      Ok expr -> case evalTypeOfM rho expr of
        Left  e -> show e
        Right t -> show t
      Bad e -> show $ ParserError e
  go (':' : 'v' : ' ' : s) = undefined
  go s                     = undefined

moduleLoadingError :: InterpreterError -> IO ()
moduleLoadingError e =
  print
    $  "Module loading failed, starting in a default environment. The error: "
    ++ show e

showResult :: IOWithInterpreterError Env -> IO ()
showResult rhoEIO = do
  rhoE <- runExceptT rhoEIO
  rho  <- case rhoE of
    Left  e         -> moduleLoadingError e >> return (baseEnv [])
    Right rho@Env{} -> return rho
  interactiveMode rho

runInterperter :: IOWithInterpreterError Env -> IO ()
runInterperter rhoEIO = do
  rhoE <- runExceptT rhoEIO
  case rhoE of
    Left  e         -> print e >> exitFailure
    Right rho@Env{} -> interactiveMode rho
  undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> runInterperter $ return $ baseEnv []
    [fileName] -> runInterperter $ processModule fileName (readFile fileName)
    _          -> usage
