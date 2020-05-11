import           System.Environment
import           Control.Monad.Except
import           System.IO
import qualified Data.Map                      as Map
import           System.Exit
import           Error
import           ProcessFile
import           Types

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  (no arguments)  Read input from stdin,"
    , "  (file)          Get file content."
    ]
  exitFailure

mainFunction :: FName
mainFunction = Prefix "main"

printErrAndExit :: Show a => a -> IO ()
printErrAndExit x = hPrint stderr x >> exitFailure

showResult :: IOWithInterpreterError Env -> IO ()
showResult rhoEIO = do
  rhoE <- runExceptT rhoEIO
  case rhoE of
    Left e -> printErrAndExit e
    Right Env { localFunctions = lFunctions, fenv = rhoF } ->
      if mainFunction `elem` lFunctions
        then case rhoF Map.! mainFunction of
          v@TVal{} -> print v
          _ -> printErrAndExit "No main function defined, but it was declared."
        else printErrAndExit "No main function declared."

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> showResult $ processModule "<stdin>" getContents
    [fileName] -> showResult $ processModule fileName (readFile fileName)
    _          -> usage
