maxFactor n = n `quot` (minFactor n 2)
minFactor n x = if (n `mod` x == 0) then x else minFactor n (x+1)


z = zip ([2..10000]) $ map maxFactor [2..10000]


p :: (Show a) => [a] -> String
p [] = ""
p (x:xs) = (show x) ++ ";\n" ++ (p xs)