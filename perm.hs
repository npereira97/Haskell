perm :: [a] -> [[a]]
perm [] = []
perm (x:[]) = [[x]]
perm (x:xs) = let xs' = perm(xs) in map (\(a,b) -> a ++ [x] ++ b) $ concat $ (map part $ map (\x -> (x,[])) xs')


part :: ([a],[a]) -> [([a],[a])]
part ([],xs) = [(xs,[])]
part ((x:xs),ys) = (ys,x:xs) : part (xs, ys ++ [x]) 


main = do 
		print $ perm "Neil Pereira"