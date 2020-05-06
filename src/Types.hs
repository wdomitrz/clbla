module Types where
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )

-- The [Val] list is reversed in VNamed!
data Val = VNamed CName [Val] | VFun (Val -> Val)
data TVal = TVal {onlyType::Type, onlyVal::Val} | NoVal {onlyType:: Type}
newtype AType = AType {numOfParams :: Int}
  deriving (Eq, Ord, Show, Read)

data Res = RObj TName CName [Res] | RFun Type Type
  deriving (Eq, Ord, Show, Read)

type TDecls = Map TName AType
type FDefs = Map FName TVal
data Env = Env {exts :: [Ext], localNames :: Set VName, tenv :: TDecls, fenv :: FDefs}

type VName = String -- type variable name
type MName = String -- module name
type FName = VName -- function (variable) name
type TName = VName -- type name
type CName = VName -- constructor name
type Ext = String -- extension
type Imp = String -- import
type TId = Int -- type variable id

data Module = Module MName [Ext] [Imp] Defs
  deriving (Eq, Ord, Show, Read)

data Type = TPoly VName | TVar TId | TNamed TName [Type] | Type :-> Type
  deriving (Eq, Ord, Show, Read)

data TConstr = TConstr CName [Type]
  deriving (Eq, Ord, Show, Read)

data TDef = TDef TName [VName] [TConstr]
  deriving (Eq, Ord, Show, Read)

data FDecl = FDecl FName Type
  deriving (Eq, Ord, Show, Read)

data FDef = FDef {fname::FName, fexp::Exp}
          | FDefWh {fname::FName, fexp::Exp, fwhere::Defs}
  deriving (Eq, Ord, Show, Read)

data Defs = Defs [TDef] [FDecl] [FDef]
  deriving (Eq, Ord, Show, Read)

data Exp = EVar FName | ELet Defs Exp | EApp Exp Exp
  deriving (Eq, Ord, Show, Read)
