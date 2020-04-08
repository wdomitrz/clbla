import           Prelude                 hiding ( pred
                                                , Bool
                                                , True
                                                , False
                                                , (>=)
                                                , (<)
                                                , (<=)
                                                , (>)
                                                , (==)
                                                , (/=)
                                                , not
                                                , or
                                                , and
                                                , (-)
                                                , (+)
                                                , (*)
                                                , (/)
                                                , (^)
                                                , (<>)
                                                , fst
                                                , snd
                                                )

data Pair a b = Pair a b

(<>) :: a -> b -> Pair a b
(<>) = Pair

foldPair :: (a -> b -> c) -> Pair a b -> c
foldPair f (Pair x y) = f x y

fst :: Pair a b -> a
fst = foldPair k

snd :: Pair a b -> b
snd = foldPair (c k)

data Bool = True | False deriving Show
foldBool :: a -> a -> Bool -> a
foldBool x _ True  = x
foldBool _ x False = x

if' :: Bool -> a -> a -> a
if' = c (b c foldBool)

not :: Bool -> Bool
not = foldBool False True

and :: Bool -> Bool -> Bool
and = c foldBool False

or :: Bool -> Bool -> Bool
or = b not `b` (b' not `b` (and `b` not))

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
foldNat _ x Zero  = x
foldNat f x (S n) = f (foldNat f x n)

(+) :: Nat -> Nat -> Nat
(+) = foldNat S

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
w :: (a -> a -> b) -> a -> b
w = s s (k i)

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
(<=) = b isZero `b` (-)
(>=) :: Nat -> Nat -> Bool
(>=) = c (<=)
(<) :: Nat -> Nat -> Bool
(<) = (<=) `b` S
(>) :: Nat -> Nat -> Bool
(>) = c (<)

(*) :: Nat -> Nat -> Nat
(*) = c foldNat zero `b` (+)
(/) :: Nat -> Nat -> Nat
(/) =
    let go :: Nat -> Nat -> Nat -> Nat
        go = s (s `b` b b `b` c (if' `b` isZero))
               (c c i `b` b s `b` b' (-) `b` b `b` go `b` S)
    in  w `b` b (go zero) `b` (-) `b` S

(^) :: Nat -> Nat -> Nat
(^) = c foldNat one `b` (*)

factorial :: Nat -> Nat
-- I can also use Pair
factorial = fst `b` foldNat go (Pair one one)
  where
    go :: Pair Nat Nat -> Pair Nat Nat
    go = s (Pair `b` lift2 (*) fst snd) (S `b` snd)

lift2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
lift2 = b (b w `b` c (b `b` b')) `b` b

fib :: Nat -> Nat
fib = fst `b` foldNat (foldPair go) (Pair zero one)
  where
    go :: Nat -> Nat -> Pair Nat Nat
    go = s Pair `b` (+)

ackerman' :: Nat -> Nat -> Nat
ackerman' Zero  n     = S n
ackerman' (S m) Zero  = ackerman m one
ackerman' (S m) (S n) = ackerman m (ackerman (S m) n)


ackerman :: Nat -> Nat -> Nat
ackerman = fst . foldNat go (Pair S one)
  where
    go :: Pair (Nat -> Nat) Nat -> Pair (Nat -> Nat) Nat
    go = lift2 Pair go' (S `b` snd)
    go' = s (s `b` c (if' `b` isZero) `b` c fst one)
            (lift2 b fst (b' pred `b` ackerman `b` snd))
