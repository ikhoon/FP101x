
last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x : xs) = drop' (n - 1) xs


(++++) :: [a] -> [a] -> [a]
[] ++++ ys = ys
(x : xs) ++++ ys = x : (xs ++++ ys)
