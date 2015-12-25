import Data.Char

factor n = [ x | x <- [1..n], n `mod` x == 0]

prime x = factor x == [1,x]

primes n = [ x | x <- [1..n], prime x]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i' | (x', i') <- zip xs [0..(length xs - 1)], x == x']

replicate' n x = [ x | _ <- [1..n] ]
                  
pytha :: Int -> [(Int, Int, Int)]
pytha n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], sum (factor x) - x == x]


find :: Eq a => a -> [(a, b)] -> [b]
find k ts = [ v | (k', v) <- ts, k == k']

positions' x xs = find x (zip xs [0..n])
                 where n = length xs - 1

scalaproduct xs ys = sum [x * y | (x, y) <- zip xs ys]



let2int :: Char -> Char -> Int
let2int c from =  ord c - ord from

int2let n from = chr (n + ord from)


shift :: Int -> Char -> Char
shift n c
    | isAlpha c = int2let((let2int c f + n) `mod` 26) f
    | otherwise = c
    where f = if isLower c then 'a' else 'A'

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]


infinity = 1 : [ x + 1 | x <- infinity ]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x, y) <- zip xs ys]
