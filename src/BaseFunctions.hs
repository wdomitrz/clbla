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

createS, createK :: TVal
createS = undefined
createK = undefined

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
  getElem ms (VNamed cname vs') = foldl go' (ms Map.! cname) vs'
  getElem _ _ =
    error
      "Type check failed - last param to the eliminator has to be an instance of a corresponding type."
  go' :: Val -> Val -> Val
  go' (VFun f) x = f x
  go' _        _ = error
    "Type failed - non-last parameter to the eliminator has to be a function"
  go :: TConstr -> (Map CName Val -> Val) -> Map CName Val -> Val
  go (TConstr cname _) acc ms = VFun (\v -> acc (Map.insert cname v ms))
createFold = undefined
