import Data.List
import Multi hiding(main)

n = 4
g = groupBy (\x y -> (elem x ['0'..'9']) && (elem y ['0'..'9']) )


f :: String -> Entry Int
f "_" = Possible [1..n*n]
f x = Fixed ((read::String ->Int) x)

main = do
    input <- mapM (\x -> getLine) [1..n*n]
    let board = map f $ concat $ map (filter (/= " ")) $ map g input
    --putStrLn $ myShow $ board
    mapM_ (putStrLn) $ map myShow $ (pfinal.pfinal.pfinal.pfinal) [board]
    --mapM_ (putStrLn) $ map myShow $ (fixedPoint pfinal) [board]
    return()