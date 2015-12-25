import Prelude hiding ((&&))

abs' :: Int -> Int
abs' x = if x >= 0 then x else -x

isOdd :: Int -> Bool
isOdd x = (x `mod` 2) == 1

safetail =
    \xs ->
        case xs of
          [] -> []
          (_:xs) -> xs
                

a && b = if a then if b then True else False else False

remove n xs = take n xs ++ drop (n + 1) xs

funct :: Int -> [a] -> [a]
funct n xs = take (n + 1) xs ++ drop n xs

integer' :: Integer -> Integer
integer' x = x
