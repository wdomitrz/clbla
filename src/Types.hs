module Types where
import           Control.Applicative            ( (<$>) )
import           Data.Map                       ( Map )
import           Data.Set                       ( Set )

data Val = VNamed CName [Val] | VFun (Val -> Val)
instance Show Val where
  showsPrec _ (VFun _) = showString "function"
  showsPrec d (VNamed (Prefix name) vs)
    | null vs = showString name
    | otherwise = showParen
      (d > 1)
      (showString name . showString " " . foldl
        (\acc -> ((acc . showString " ") .))
        (showsPrec 2 $ head vs)
        (showsPrec 2 <$> tail vs)
      )
  showsPrec d (VNamed (Infix name) vs) = showParen
    (d > 0)
    ( showsPrec (if d == 0 then 1 else 2) (head vs)
    . showString " "
    . showString name
    . showString " "
    . foldr
        (.)
        (showsPrec (if null $ tail $ tail vs then 0 else 1) $ head $ tail vs)
        (fmap (showsPrec 1) (tail $ tail vs))
    )

data TVal = TVal {onlyType::Type, onlyVal::Val} | NoVal {onlyType:: Type}
instance Show TVal where
  show TVal { onlyVal = v } = show v
  show NoVal{}              = "No value"

data AType = AType {numOfParams :: Int, params :: [VName],
                    constrs :: [TConstr]}
-- constrs is there to distinguish two types form two different modules
  deriving (Eq, Ord, Show, Read)

data Res = RObj TName CName [Res] | RFun Type Type
  deriving (Eq, Ord, Show, Read)

type TDecls = Map TName AType
type FDefs = Map FName TVal
data Env = Env {exts :: [Ext], localTypes :: Set TName,
                localFunctions :: Set FName, tenv :: TDecls, fenv :: FDefs}
  deriving Show

data PIName = Prefix String | Infix String
  deriving (Eq, Ord, Read)
instance Show PIName where
  show (Prefix n) = n
  show (Infix  n) = "(" ++ n ++ ")"

type VName = String -- type variable name
type MName = String -- module name
type FName = PIName -- function (variable) name
type TName = VName -- type name
type CName = PIName -- constructor name
type Ext = String -- extension
type Imp = String -- import
type TId = Integer -- type variable id

data Module = Module MName [Ext] [Imp] Defs
  deriving (Eq, Ord, Show, Read)

data Type = TPoly VName | TVar TId | TNamed TName [Type] | Type :-> Type
  deriving (Eq, Ord, Read)
instance Show Type where
  showsPrec _ (TPoly name) = showString name
  showsPrec _ (TVar  tid ) = showString "_t" . shows tid
  showsPrec d (TNamed name ts)
    | null ts = showString name
    | otherwise = showParen
      (d > 1)
      (showString name . showString " " . foldl
        (\acc -> ((acc . showString " ") .))
        (showsPrec 2 $ head ts)
        (showsPrec 2 <$> tail ts)
      )
  showsPrec d (t1 :-> t2) =
    showParen (d > 0) (showsPrec 1 t1 . showString " -> " . shows t2)

data TConstr = TConstr CName [Type]
  deriving (Eq, Ord, Show, Read)

data TDef = TDef TName [VName] [TConstr]
  deriving (Eq, Ord, Show, Read)

data FDecl = FDecl FName Type
  deriving (Eq, Ord, Show, Read)

data FDef = FDef {fname::FName, fexp::Exp}
          | FDefWh {fname::FName, fexp::Exp, fwhere::Defs}
  deriving (Eq, Ord, Show, Read)

data Defs = Defs {defsTDef :: [TDef], defsFDecl :: [FDecl], defsFDef :: [FDef]}
  deriving (Eq, Ord, Show, Read)

data Exp = EVar FName | ELet Defs Exp | EApp Exp Exp
  deriving (Eq, Ord, Show, Read)

data AnnotExp a = AEVar {aEVarFname :: FName, annot :: a}
                | AELet {aEVarDefs :: Defs, aEVarExp :: Exp, annot :: a}
                | AEApp {aEVarE1 :: Exp, aEVarE2 :: Exp, annot :: a}
