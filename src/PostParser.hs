module PostParser where
import qualified Parser.AbsClbla               as Abs
import           Parser.AbsClbla                ( UIdent(..)
                                                , LIdent(..)
                                                )
import           Types


procModule :: FilePath -> Abs.Programme -> Module
procModule name (Abs.Prog exts imps env) = Module
  name
  (fmap (\(Abs.Ext (UIdent ext)) -> ext) exts)
  (fmap (\(Abs.Imp (UIdent imp)) -> imp) imps)
  (procEnv env)

procEnv :: Abs.Environment -> Defs
procEnv = go [] [] [] . (\(Abs.Env is) -> is)
 where
  go :: [TDef] -> [FDecl] -> [FDef] -> [Abs.Instruction] -> Defs
  go tdfs fdcs fdfs [] = Defs tdfs fdcs fdfs
  go tdfs fdcs fdfs (Abs.InstructionTypeDefinition x : is) =
    go (procTDef x : tdfs) fdcs fdfs is
  go tdfs fdcs fdfs (Abs.InstructionFunctionDeclaration x : is) =
    go tdfs (procFDecl x : fdcs) fdfs is
  go tdfs fdcs fdfs (Abs.InstructionFunctionDefinition x : is) =
    go tdfs fdcs (procFDef x : fdfs) is

procTDef :: Abs.TypeDefinition -> TDef
procTDef (Abs.TDef (UIdent name) ps cs) =
  TDef name (fmap (\(Abs.ParamVar (LIdent vn)) -> vn) ps) (fmap procConstr cs)

procFDecl :: Abs.FunctionDeclaration -> FDecl
procFDecl (Abs.FDecl name tp) = FDecl (procFBName name) (procType tp)

procFDef :: Abs.FunctionDefinition -> FDef
procFDef (Abs.FDef name exp) = FDef (procFBName name) (procExp exp)

procConstr :: Abs.TypeConstructor -> TConstr
procConstr (Abs.TConst (UIdent name) ts) = TConstr name (fmap procType ts)
procConstr (Abs.TInfixConst t (Abs.TIConstName (Abs.InfixFunctionNameF name)) ts)
  = TConstr name (fmap procType (t : ts))

procType :: Abs.Type -> Type
procType (Abs.TFun   t1            t2) = procType t1 :-> procType t2
procType (Abs.TNamed (UIdent name) ts) = TNamed name (fmap procType ts)
procType (Abs.TVar (LIdent name)     ) = TVar name

procExp :: Abs.Expression -> Exp
procExp (Abs.ELet env e                      ) = ELet (procEnv env) (procExp e)
procExp (Abs.EApp e1  e2                     ) = EApp (procExp e1) (procExp e2)
procExp (Abs.EVar (Abs.FName   name         )) = EVar $ procFBName name
procExp (Abs.EVar (Abs.FTCName (UIdent name))) = EVar name
procExp (Abs.EVar (Abs.FITCName (Abs.TIConstName (Abs.InfixFunctionNameF name))))
  = EVar name
procExp (Abs.EOpA e1 (Abs.InfixFunctionNameA name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpB e1 (Abs.InfixFunctionNameB name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpC e1 (Abs.InfixFunctionNameC name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpD e1 (Abs.InfixFunctionNameD name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpE e1 (Abs.InfixFunctionNameE name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpF e1 (Abs.InfixFunctionNameF name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpG e1 (Abs.InfixFunctionNameG name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpH e1 (Abs.InfixFunctionNameH name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpI e1 (Abs.InfixFunctionNameI name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpJ e1 (Abs.InfixFunctionNameJ name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)
procExp (Abs.EOpK e1 (Abs.InfixFunctionNameK name) e2) =
  EApp (EApp (EVar name) (procExp e1)) (procExp e2)

procFBName :: Abs.FunctionBaseName -> FName
procFBName (Abs.FBName (LIdent name)) = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameA (Abs.InfixFunctionNameA name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameB (Abs.InfixFunctionNameB name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameC (Abs.InfixFunctionNameC name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameD (Abs.InfixFunctionNameD name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameE (Abs.InfixFunctionNameE name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameG (Abs.InfixFunctionNameG name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameH (Abs.InfixFunctionNameH name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameI (Abs.InfixFunctionNameI name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameJ (Abs.InfixFunctionNameJ name)))
  = name
procFBName (Abs.FIBName (Abs.FunctionInfixNameInfixFunctionNameK (Abs.InfixFunctionNameK name)))
  = name
