module BaseFunctions where
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Monad                  ( liftM2 )
import           Data.List                      ( (\\) )
import           Types

polyPNamesPref, polyPNamesSuf, polyPNames :: [String]
polyPNamesPref = fmap (: []) ['a' .. 'z']
polyPNamesSuf = "" : fmap show [(1 :: Integer) ..]
polyPNames = liftM2 (flip (++)) polyPNamesSuf polyPNamesPref
getFreeParamName :: [String] -> String
getFreeParamName = head . (polyPNames \\)

algTType :: TDef -> Type
algTType (TDef name vs _) = TNamed name (fmap TPoly vs)

unVFun :: Val -> (Val -> Val)
unVFun (VFun f) = f
unVFun x =
  error $ "Type checker failed - an object should be a function and is: " ++ show x
unVNamed :: Val -> (CName, [Val])
unVNamed (VNamed cname vs) = (cname, vs)
unVNamed x =
  error
    $  "Type checker failed - an object should be a named object and is: "
    ++ show x

createS, createK :: TVal
createS = TVal tp val
 where
  tp :: Type
  tp =
    (TPoly "a" :-> (TPoly "b" :-> TPoly "c"))
      :-> ((TPoly "a" :-> TPoly "b") :-> (TPoly "a" :-> TPoly "c"))
  val :: Val
  val = VFun (\x -> VFun (\y -> VFun (\z -> unVFun (unVFun x z) (unVFun y z))))
createK = TVal tp val
 where
  tp :: Type
  tp = TPoly "a" :-> (TPoly "b" :-> TPoly "a")
  val :: Val
  val = VFun (VFun . const)

createContructor :: Type -> TConstr -> TVal
createContructor baseTp (TConstr cname ts) = TVal tp val
 where
  tp :: Type
  tp = foldr (:->) baseTp ts
  val :: Val
  val = foldr go (VNamed cname . reverse) ts []
  go :: a -> ([Val] -> Val) -> [Val] -> Val
  go _ acc vs = VFun (\v -> acc (v : vs))

createElim, createFold :: TDef -> TVal
createElim td@(TDef _ vs cs) = TVal tp val
 where
  resTp :: Type
  resTp = TPoly (getFreeParamName vs)
  tp :: Type
  tp = foldr (:->)
             (algTType td :-> resTp)
             (fmap (\(TConstr _ ts) -> foldr (:->) resTp ts) cs)
  val :: Val
  val = foldr go (VFun . getElem) cs Map.empty
  getElem :: Map CName Val -> Val -> Val
  getElem ms x =
    let (cname, vs') = unVNamed x in foldl unVFun (ms Map.! cname) vs'
  go :: TConstr -> (Map CName Val -> Val) -> Map CName Val -> Val
  go (TConstr cname _) acc ms = VFun (\v -> acc (Map.insert cname v ms))

createFold td@(TDef _ vs cs) = TVal tp val
 where
  resTp :: Type
  resTp = TPoly (getFreeParamName vs)
  mt :: Type
  mt = algTType td
  tp :: Type
  tp = foldr
    (:->)
    (mt :-> resTp)
    (fmap
      (\(TConstr _ ts) ->
        foldr (:->) resTp (fmap (\t -> if t == mt then resTp else t) ts)
      )
      cs
    )
  -- indicators which constructors parameters should be replaced with fold call
  iwcpsbrwfc :: Map CName [Bool]
  iwcpsbrwfc =
    Map.fromList $ fmap (\(TConstr cname ts) -> (cname, fmap (tp ==) ts)) cs
  val :: Val
  val = foldr go (VFun . getElem) cs Map.empty
  getElem :: Map FName Val -> Val -> Val
  getElem mp x = getVal mp' fwpin go' Map.! fwpin
   where
    cname :: CName
    vs' :: [Val]
    (cname, vs') = unVNamed x
    mp' :: Map FName Val
    mp' = foldr (uncurry Map.insert) mp $ zip vNames vs'
    go' :: Exp
    go' = foldl
      (\acc (b, n) ->
        EApp acc (if b then EApp (EVar fwpin) (EVar n) else EVar n)
      )
      (EVar cname) -- we get access to a function meant do deal with given constructor using the name of this constructor
      (zip (iwcpsbrwfc Map.! cname) vNames)
  -- fold with parameters inner name
  fwpin :: FName
  fwpin  = "__fold__"
  vNames = fmap (('v' :) . show) [(1 :: Integer) ..]
  go :: TConstr -> (Map CName Val -> Val) -> Map CName Val -> Val
  go (TConstr cname _) acc ms = VFun (\v -> acc (Map.insert cname v ms))

getVal :: Map FName Val -> FName -> Exp -> Map FName Val
getVal rho n e = let rho' = Map.insert n (go rho' e) rho in rho'
 where
  go :: Map FName Val -> Exp -> Val
  go rho' (EVar n'    ) = rho' Map.! n'
  go rho' (EApp e1 e2) = unVFun (go rho' e1) $ go rho' e2
  go _    (ELet _  _ ) = error "You cannot use let in here"
