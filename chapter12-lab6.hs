import Prelude hiding (Monoid, Foldable, mappend, mempty, foldMap)
------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
root (x :> xs) = x

children :: Rose a -> [Rose a]
children (x :> xs) = xs

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

tree0 = 'x' :> map (flip (:>) []) ['a'..'x']
tree1 = 'x' :> map (\c -> c :> []) ['a'..'A']

     
ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (x :> []) = 1
size (x :> xs) = 1 + (foldr (\n acc -> size n + acc) 0 xs)

tree3 = 1 :> map (\c -> c :> []) [1..5]
tree4 = 1 :> map (\c -> c :> []) [1..5]

leaves :: Rose a -> Int
leaves (x :> []) = 1
leaves (x :> xs) = (foldr (\n acc -> leaves n + acc) 0 xs)

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
    fmap f (x :> []) = (f x) :> []
    fmap f (x :> xs) = f x :> map (\n -> fmap f n) xs

f' r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)
  
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)

unSum :: Sum a -> a
unSum (Sum n) = n
unProduct :: Product a -> a
unProduct (Product n) = n

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> f a -> m
  foldMap g = fold . fmap g
  
instance Foldable Rose where
  fold (x :> []) = mappend x mempty
  fold (x :> xs) = mappend x (foldr (\a b -> mappend (fold a) b)  mempty xs)
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum xs = unSum $ foldMap Sum xs
fproduct xs = unProduct $ foldMap Product xs

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

