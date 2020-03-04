{-# LANGUAGE BangPatterns #-}




pow :: Int -> (Int -> Int)
pow 0 = (\x -> 1)
pow 1 = (\x -> x)
pow n = let !f = (pow (n-1)) in
            (\x -> x * (f x))



pow' :: Int -> (Int -> Int)
pow' 0 = (\x -> 1)
pow' 1 = (\x -> x)
pow' n = (\x -> (x* (pow' (n-1) x)))