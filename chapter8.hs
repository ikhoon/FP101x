import Data.Char
import Control.Monad

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = putStr' xs >> putChar '\n'

putStrLn'' :: String -> IO ()
putStrLn'' xs = putStr' xs >>= \x -> putChar '\n'

putStrLn''' :: String -> IO ()
putStrLn''' [] = putChar '\n'
putStrLn''' xs = putStr' xs >> putStrLn''' ""


getLine' :: IO String
getLine' = get ""

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _ -> get(xs ++ [x])


interact' :: (String -> String) -> IO ()
interact' f = do input <- getLine'
                 putStrLn' $ f input


upper' :: String -> String
upper' [] = []
upper' (x:xs) = toUpper x : upper' xs

sequence_' [] = return []
sequence_' (m:ms) = m >>= \_ -> sequence_' ms


sequence_1' [] = return ()
sequence_1' (m:ms) = (foldl (>>) m ms) >> return ()

sequence_2' ms = (foldl (>>) (return()) ms)

sequence_3' [] = return ()
sequence_3' (m:ms) =  m >> sequence_3' ms

sequence_4' ms = (foldr (>>) (return()) ms)


sequence' :: (Monad m) => [m a] -> m [a]
sequence' [] = return []
sequence' (x:ms) = x >>=
                   \a ->
                       do as <- sequence' ms
                          return (a:as)

-- sequence1' :: (Monad m) => [m a] -> m [a]
-- sequence1' ms = foldr func (return ()) ms
--     where
--       func :: (Monad m) => m a -> [m a] -> m [a]
--       func m acc = do x <- m
--                       xs <- acc
--                       return (x:xs)
           
-- sequence2' :: (Monad m) => [m a] -> m [a]
-- sequence2' ms = foldr func (return []) ms
--     where
--       func :: (Monad m) => m a -> m [a] -> m [a]
--       func m acc = (m:acc)

-- sequence3' :: (Monad m) => [m a] -> m [a]
-- sequence3' [] = return []
-- sequence3' (m:ms) = return (a:as)
--                     where
--                       a <- m
--                       as <- sequence3' ms

sequence4' :: (Monad m) => [m a] -> m [a]
sequence4' ms = foldr func (return []) ms
    where
      func :: (Monad m) => m a -> m [a] -> m [a]
      func m acc =
          do x <- m
             xs <- acc
             return (x:xs)
        
-- sequence5' :: (Monad m) => [m a] -> m [a]
-- sequence5' [] = return []
-- sequence5' (m:ms) = m >>= \a ->
--                     as <- sequence' ms
--                     return (a:as)

sequence6' :: (Monad m) => [m a] -> m [a]
sequence6' [] = return []
sequence6' (m:ms) =
    do x <- m
       xs <- sequence6' ms
       return (x:xs)
  
mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)


mapM1' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM1' f [] = return []
mapM1' f (a : as) =
    f a >>= \b -> mapM1' f as >>= \bs -> return (b:bs)

mapM2' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM2' f as = sequence_' (map f as)

mapM3' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM3' f [] = return []
mapM3' f (a:as) =
    f a >>=
      \ b ->
        do bs <- mapM3' f as
           return (b:bs)
                 

foldLeftM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM' f a [] = return a
foldLeftM' f a (x:xs) = (f a x) >>= \b ->
                        foldLeftM' f b xs

fold'' :: (a -> b -> a) -> a -> [b] -> a
fold'' f a [] = a
fold'' f a (x:xs) = fold'' f (f a x) xs


foldRightM' :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldRightM' f b [] = return b
foldRightM' f b (a:as) =  foldRightM' f b as >>=
                          \ x -> f a x
 
             
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f b [] = b
foldr'' f b (x:xs) = f x (foldr'' f b xs)


liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = do x <- m
                return (f x)

liftM1' :: (Monad m) => (a -> b) -> m a -> m b
liftM1' f m = m >>= \a -> return (f a)
                            
liftM2' :: (Monad m) => (a -> b) -> m a -> m b
liftM2' f m = m >>= \a -> m >>= \b -> return (f a)


liftM3' :: (Monad m) => (a -> b) -> m a -> m b
liftM3' f m = mapM' f [m]
