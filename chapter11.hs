fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
fib n = fibs !! n

largeFib :: Integer
largeFib = head $ dropWhile (<=1000) fibs
            
data Tree a = Leaf 
            | Node (Tree a) a (Tree a)

repeatTree :: a -> Tree a
repeatTree a = Node (repeatTree a) a (repeatTree a)

printTree :: Tree Char -> IO () 
printTree (Node l a r) =
    do
      putChar '('
      printTree l
      putChar '.'
      putChar a
      putChar '.'
      printTree r
      putChar ')'
