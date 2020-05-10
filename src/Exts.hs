module Exts where
import           Types

elimOff, foldOn :: Ext
elimOff = "NElim"
foldOn = "FOn"

enabled :: Ext -> Env -> Bool
enabled = (. exts) . elem
