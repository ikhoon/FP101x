unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
       | p x = []
       | otherwise = h x : unfold p h t (t x)
