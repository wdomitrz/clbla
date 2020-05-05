module Utils where
import           Control.Monad.Trans.Reader
import           Control.Monad                  ( foldM )
import           Data.List                      ( nub )
import           Data.Foldable                  ( toList )

allUnique :: (Foldable t, Eq a) => t a -> Bool
allUnique xs = let xs' = toList xs in xs' == nub xs'
