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
  error $ "Type failed - an object should be a function and is: " ++ show x
unVNamed :: Val -> (CName, [Val])
unVNamed (VNamed cname vs) = (cname, vs)
unVNamed x =
  error
    $  "Type check failed - an object should be a named object and is: "
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
createFold = undefined
