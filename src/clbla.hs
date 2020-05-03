import           System.Environment
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map                      as Map
import           Parser.ParClbla
import           System.Exit
import           Error
import           ProcessModule

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

runIterpreter = runExceptT

main :: IO ()
main = do
  args        <- getArgs
  toInterpret <- case args of
    []         -> parseModule "<stdin>" getContents
    [fileName] -> parseFile fileName
  case runReader (runExceptT toInterpret) Map.empty of
    Left  e -> print e
    Right r -> print r
