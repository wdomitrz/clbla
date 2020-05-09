module Types where
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )

data Val = VNamed CName [Val] | VFun (Val -> Val)
instance Show Val where
  show (VNamed cname vs) = cname ++ " " ++ show vs
  show (VFun _         ) = "Function"
data TVal = TVal {onlyType::Type, onlyVal::Val} | NoVal {onlyType:: Type}
  deriving Show
data AType = AType {numOfParams :: Int, constrs :: [TConstr]}
-- constrs is there to distinguish two types form two different modules
  deriving (Eq, Ord, Show, Read)

data Res = RObj TName CName [Res] | RFun Type Type
  deriving (Eq, Ord, Show, Read)

type TDecls = Map TName AType
type FDefs = Map FName TVal
data Env = Env {exts :: [Ext], localTypes :: Set TName,
                localFunctions :: Set FName, tenv :: TDecls, fenv :: FDefs}
  deriving Show

type VName = String -- type variable name
type MName = String -- module name
type FName = VName -- function (variable) name
type TName = VName -- type name
type CName = VName -- constructor name
type Ext = String -- extension
type Imp = String -- import
type TId = Integer -- type variable id

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
