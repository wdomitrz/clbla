import           System.Environment
import           Control.Monad.Except
import qualified Data.Map                      as Map
import           System.Exit
import           Error
import           ProcessFile
import           Types

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

mainFunction :: String
mainFunction = "main"

showResult :: IOWithInterpreterError Env -> IO ()
showResult rhoEIO = do
  rhoE <- runExceptT rhoEIO
  case rhoE of
    Left e -> print e >> exitFailure
    Right Env { localFunctions = lFunctions , fenv = rhoF } ->
      if mainFunction `elem` lFunctions
        then case rhoF Map.! mainFunction of
          v@TVal{} -> print v
          _ ->
            print "No main function defined, but it was declared" >> exitFailure
        else print "No main function defined" >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> showResult $ processModule "<stdin>" getContents
    [fileName] -> showResult $ processFile fileName
    _          -> usage
