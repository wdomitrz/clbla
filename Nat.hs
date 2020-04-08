import Prelude hiding (pred,Bool,True,False,(>=),(<),(<=),(>),(==),(/=),not,or,and,(-))

data Bool = True | False deriving Show
foldBool :: a -> a -> Bool -> a
foldBool x _ True = x
foldBool _ x False = x

if' :: Bool -> a -> a -> a
if' = c (b c foldBool)

not :: Bool -> Bool
not = foldBool False True

and :: Bool -> Bool -> Bool
and = c foldBool False

or :: Bool -> Bool -> Bool
or = (b not) `b` ((b' not) `b` (and `b` not))

data Nat = S Nat | Zero deriving Show

zero :: Nat
zero = Zero
one :: Nat
one = S Zero
two :: Nat
two = S one
three :: Nat
three = S two
four :: Nat
four = S three

foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ x Zero = x
foldNat f x (S n) = f (foldNat f x n)

add' :: Nat -> Nat -> Nat
add' = foldNat S

b :: (b -> c) -> (a -> b) -> a -> c
b = (.)
k :: a -> b -> a
k = const
c :: (a -> b -> c) -> b -> a -> c
c = flip
s :: (a -> b -> c) -> (a -> b) -> a -> c
s = (<*>)
i :: a -> a
i = id
b' :: (a -> b) -> (b -> c) -> a -> c
b' = c b

data NatsPair = PNats Nat Nat
foldNatsPair :: (Nat -> Nat -> a) -> NatsPair -> a
foldNatsPair f (PNats x y) = f x y
pred :: Nat -> Nat
pred = fstNat . foldNat (s PNats S . sndNat) (PNats Zero Zero)
    where
        fstNat :: NatsPair -> Nat
        fstNat = foldNatsPair k
        sndNat :: NatsPair -> Nat
        sndNat = foldNatsPair (c k)


(-) :: Nat -> Nat -> Nat
(-) = foldNat pred

isZero :: Nat -> Bool
isZero = foldNat (k False) True

(<=) :: Nat -> Nat -> Bool
(<=) = (isZero .) . (-)
(>=) :: Nat -> Nat -> Bool
(>=) = c (<=)
(<) :: Nat -> Nat -> Bool
(<) = c (>)
(>) :: Nat -> Nat -> Bool
(>) = ((>=) .) .
