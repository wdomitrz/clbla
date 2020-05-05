module Utils where
import           Data.List                      ( nub )
import           Data.Foldable                  ( toList )

allUnique :: (Foldable t, Eq a) => t a -> Bool
allUnique xs = let xs' = toList xs in xs' == nub xs'
