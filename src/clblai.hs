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

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

pExp :: String -> Err Exp
pExp =
  (((\(Module _ _ _ (Defs _ _ (fdf : _))) -> fexp fdf) . procModule "a") <$>)
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

interactiveMode :: Env -> IO ()
interactiveMode rho = interact (unlines . fmap go . lines)
 where
  go :: String -> String
  go s = case pExp ("zxc = " ++ s) of
    Ok expr -> case evalTypeOfM rho expr of
      Left  e -> show e
      Right t -> show t
    Bad e -> "Parser Error:\t" ++ e

showResult :: IOWithInterpreterError Env -> IO ()
showResult rhoEIO = do
  rhoE <- runExceptT rhoEIO
  case rhoE of
    Left  e         -> print e >> exitFailure
    Right rho@Env{} -> interactiveMode rho

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> showResult $ processFile fileName
    _          -> usage
