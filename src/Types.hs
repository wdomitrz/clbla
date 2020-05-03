module Types where
import           Data.Map

type MName = String -- module name
type FName = String -- function (variable) name
type TName = String -- type name
type VName = String -- type variable name
type CName = String -- constructor name
type Ext = String -- extension
type Imp = String -- import

data Module = Module MName [Ext] [Imp] Env
  deriving (Eq, Ord, Show, Read)

data Type = TPolym VName | TVar VName | TNamed TName [Type] | Type :-> Type
  deriving (Eq, Ord, Show, Read)

data TConstr = TConstr CName [Type]
  deriving (Eq, Ord, Show, Read)

data TDef = TDef TName [VName] [TConstr]
  deriving (Eq, Ord, Show, Read)

data FDecl = FDecl FName Type
  deriving (Eq, Ord, Show, Read)

data FDef = FDef FName Exp | FDefWh FName Exp Env
  deriving (Eq, Ord, Show, Read)

data Env = Env {tdefs :: [TDef], fdecls :: [FDecl], fdefs :: [FDef]}
  deriving (Eq, Ord, Show, Read)

data Exp = EVar FName | ELet Env Exp | EApp Exp Exp
  deriving (Eq, Ord, Show, Read)
