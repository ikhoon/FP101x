module Chapter9 where

import Data.Char
import Data.List
import Unsafe.Coerce


data Nat = Zero
         | Succ Nat
           deriving Show

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

natToInteger' :: Nat -> Integer
natToInteger' (Succ n) = natToInteger n + 1
natToInteger' Zero = 0

natToInteger'' :: Nat -> Integer
natToInteger'' = head . m
    where m Zero = [0]
          m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger''' :: Nat -> Integer
natToInteger''' = \n -> genericLength [ c | c <- show n, c == 'S']
                                                 
natToInteger'''' :: Nat -> Int
natToInteger'''' = \n -> length [ c | c <- show n, c == 'S']

-- integerToNat :: Integer -> Nat
-- integerToNat 0 = Zero
-- integerToNat (n + 1) = (Succ(integerToNat n))

-- integerToNat :: Integer -> Nat
-- integerToNat n
--     = product [ (unsafeCoerce c) :: Integer | c <- show n ]
                
-- integerToNat :: Integer -> Nat
-- integerToNat n = let m = integerToNat (n - 1) in Succ m
-- integerToNat 0 = Zero

add' :: Nat -> Nat -> Nat
add' Zero n = n
add' (Succ m) n = Succ (add' n m)
        
add1' :: Nat -> Nat -> Nat
add1' (Succ m) n = Succ (add1' n m)
add1' Zero n = n
               
add2' :: Nat -> Nat -> Nat
add2' Zero n = n
add2' (Succ m) n = Succ (add2' m n)
        
add3' :: Nat -> Nat -> Nat
add3' n Zero = n
add3' n (Succ m) = Succ (add3' m n)

mult' :: Nat -> Nat -> Nat
mult' Zero Zero = Zero
mult' m (Succ n) = add' m (mult' m n)

mult'' :: Nat -> Nat -> Nat
mult'' m Zero = Zero
mult'' m (Succ n) = add' m (mult'' m n)

mult''' :: Nat -> Nat -> Nat
mult''' m Zero = Zero
mult''' m (Succ n) = add' n (mult''' m n)


-- Tree
data Tree = Leaf Integer
          | Node Tree Integer Tree

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r)
    = case compare m n of
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r

                       
class (Functor f) => Foldable f where
    fold :: (Monoid m) => f m -> m

instance Foldable [] where
    fold = foldr (<>) mempty
