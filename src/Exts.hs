module Exts where
import           Types

elimOff, foldOn, lambdaOn :: Ext
elimOff = "NElim"
foldOn = "FOn"
lambdaOn = "Pathetic"

enabled :: Ext -> Env -> Bool
enabled = (. exts) . elem
