import System.Random
import Data.List

randList :: Int -> [IO Int]
randList n = (randomRIO (0,n)) : (randList n)

data Pair a = Pair Int a deriving (Show)

instance Eq (Pair a) where
    (==) (Pair x _) (Pair y _) = x == y
instance Ord (Pair a) where
    compare (Pair x _) (Pair y _) = compare x y

shuffle :: [a] -> IO [a]
shuffle xs = do 
            let l = length xs
            rlst <- sequence $ take l $ randList (2*l) 
            let comb = zipWith (Pair) rlst xs
            let lst = map (\(Pair _ x) -> x) $ sort comb 
            return lst
                 
main = do 
        rlst <- sequence $ take 10 $ randList 100 
        let x = [1..10]
        let comb = zipWith (Pair) rlst x
        let lst = map (\(Pair _ x) -> x) $ sort comb 
        print lst


minFactor :: Int -> Int -> Int
minFactor n guess = if (n `mod` guess) == 0 then guess else (minFactor n (guess + 1))


factors :: Int -> [Int]
factors 1 = []
factors n = let t = (minFactor n 2) in (t: (factors $ n `div` t))