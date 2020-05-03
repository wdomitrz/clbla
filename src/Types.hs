module Types where
import           Data.Map

data Fun = Fun {fname :: FName, fun :: Exp}
newtype AType = AType {noParams :: Int}

data Env = Env {tenv :: Map TName AType, fenv :: Map FName Fun}

type MName = String -- module name
type FName = String -- function (variable) name
type TName = String -- type name
type VName = String -- type variable name
type CName = String -- constructor name
type Ext = String -- extension
type Imp = String -- import

data Module = Module MName [Ext] [Imp] Defs
  deriving (Eq, Ord, Show, Read)

data Type = TPolym VName | TVar VName | TNamed TName [Type] | Type :-> Type
  deriving (Eq, Ord, Show, Read)

data TConstr = TConstr CName [Type]
  deriving (Eq, Ord, Show, Read)

data TDef = TDef TName [VName] [TConstr]
  deriving (Eq, Ord, Show, Read)

data FDecl = FDecl FName Type
  deriving (Eq, Ord, Show, Read)

data FDef = FDef FName Exp | FDefWh FName Exp Defs
  deriving (Eq, Ord, Show, Read)

data Defs = Defs {tdefs :: [TDef], fdecls :: [FDecl], fdefs :: [FDef]}
  deriving (Eq, Ord, Show, Read)

data Exp = EVar FName | ELet Defs Exp | EApp Exp Exp
  deriving (Eq, Ord, Show, Read)
