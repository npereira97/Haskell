import System.Environment
import Data.List.Split
import Data.List


dim = 9
dim2 = dim*dim
dim3 = dim*dim*dim
dim4 = dim2*dim2

expandPossible :: Board a -> [Board a]
expandPossible [] = []
expandPossible (Fixed x: xs) = map (Fixed x :) $ expandPossible xs
expandPossible (Empty  : xs) = map (Empty :) $ expandPossible xs
expandPossible (Possible xs: xss) = [Fixed x : xss | x <-xs]

toBoard :: [[a]] -> Board a
toBoard [] = []
toBoard (x:xs) = case (length x) of 
                    0 -> (Empty ) : (toBoard xs)
                    1 -> (Fixed (head x) ) : (toBoard xs)
                    _ -> (Possible x ) : (toBoard xs)

board = [Possible [1,2],Fixed 3, Fixed 4,Possible [1..dim2],Possible [5,6],Fixed 2,Fixed 7 ,Fixed 6,Possible [1..dim2]]


data Entry a = Fixed a | Possible [a] | Empty deriving (Eq)

instance (Show a) => Show (Entry a) where
    show (Empty ) = "Empty"
    show (Fixed n ) =  show n
    show (Possible xs) = "Possible " ++ (show xs)

type Board a = [Entry a]

myShow :: Show a => Board a -> String
myShow board = unlines $ map show (getRow board)



makeBoard :: Board Int
makeBoard = take dim4 $ repeat (Possible [1..dim2])

makeBoard' :: Board Int
makeBoard' = [Fixed x| x <- [1..dim]]

fixedPrune :: Eq a => (Board a) -> (Board a)
fixedPrune board =  (map g board) where 
                    f (Fixed x) = True
                    f _ = False
                    ts = map (\(Fixed x) -> x) $ filter f board
                    g (Fixed x) = (Fixed x)
                    g (Empty) = Empty
                    g (Possible xs ) = case (length temp) of 
                                        0 -> Empty 
                                        1 -> Fixed (head temp ) 
                                        _ -> Possible temp 

                                        where 
                                            temp =  [x | x<-xs, not $ elem x ts] 


magic :: (Eq a) => [a] -> [a] -> Entry a
magic [] poss = Possible poss
magic (x:xs) poss = if (x `elem` poss) then Fixed x else (magic xs poss)

collectPossible :: Board a -> [[a]]
collectPossible [] = []
collectPossible (Possible xs : xss) = xs : collectPossible xss
collectPossible (_:xss) = collectPossible xss


uniqueModify :: (Eq a) => [a] -> Board a -> Board a
uniqueModify [] board = board
uniqueModify lst ((Possible xs):xss) = (magic lst xs) : (uniqueModify lst xss)
uniqueModify lst (x:xss) = x : (uniqueModify lst xss)
uniqueModify lst [] = []

uniqueSubPrune :: (Ord a,Eq a) => Board a -> Board a
uniqueSubPrune board  = uniqueModify temp board where
                    temp = map (\(x,n) -> x) $ filter (\(x,n) -> n == 1) $ map (\xs -> (head xs,length xs) ) $ group $ sort $ concat $ collectPossible board


prune :: (Ord a,Eq a) => Board a -> Board a
prune = rowPrune.colPrune.blockPrune

rowPrune :: (Ord a,Eq a) => Board a -> Board a
rowPrune board = rowToBoard $ map (uniqueSubPrune.(fixedPoint fixedPrune)) $ getRow  board

colPrune :: (Ord a,Eq a) => Board a -> Board a
colPrune board = colToBoard $ map (uniqueSubPrune.(fixedPoint fixedPrune)) $ getCol  board

blockPrune :: (Ord a,Eq a) => Board a -> Board a
blockPrune board = blockToBoard $ map (uniqueSubPrune.(fixedPoint fixedPrune)) $ getBlock  board


voir :: (Show a) => (Board a) -> IO ()
voir = (putStrLn.myShow)

getRow :: Board a -> [Board a]
getRow [] = []
getRow board = take dim2 board : getRow (drop dim2 board)

rowToBoard :: [Board a] -> Board a
rowToBoard = concat



getCol :: Board a -> [Board a]
getCol [] = (repeat [])
getCol board =  zipWith (:) (take dim2 board) $ getCol (drop dim2 board)

getBlock :: Board a -> [Board a]
getBlock  [] = []
getBlock board = sepBlock (take dim3 board) ++ getBlock(drop dim3 board )

sepBlock :: Board a -> [Board a]
sepBlock [] = (repeat [])
sepBlock board = zipWith (++) (chunksOf dim (take dim2 board)) (sepBlock (drop dim2 board))

blockToBoard :: [Board a] -> Board a
blockToBoard [] = []
blockToBoard boards = (blockJoin (take dim boards) )   ++  blockToBoard(drop dim boards)

blockJoin :: [Board a] -> Board a
blockJoin [] = []
blockJoin ([] : xs) = []
blockJoin boards = concat (map (take dim) boards) ++ (blockJoin (map (drop dim) boards))



colToBoard :: [Board a] -> Board a
colToBoard boards = concat $ getCol $ concat boards

--

transform :: Board a -> [[a]]
transform board = map f board where
                    f :: (Entry a) -> [a]
                    f (Fixed x) = [x]
                    f (Possible xs) = xs
                    f (Empty) = []

repCheck :: Eq a => Board a -> Bool
repCheck board =  (length t) /= (length $ nub t) where
                    t = (collect board)
                    collect [] = []
                    collect (Fixed x : xs) = x : (collect xs)
                    collect (_ :xs) = collect xs

repCheck' :: Eq a => Board a -> Bool
repCheck' board = go board [] where
                    go [] _ = False;
                    go ((Fixed y):xs) acc = if y `elem` acc then True else (go xs (y:acc))
                    go (_:xs) acc = go xs acc


isValid :: Eq a => (Board a) -> Bool
isValid board = and t where
                t = map f board
                f x = case x of 
                    (Empty ) -> False
                    _ -> True


check :: Eq a => Board a -> Bool
check board = (isValid board) && (and $ map (not.repCheck) $ concat $ (sequenceA [getRow,getCol,getBlock]) board)


isExpandable ::  Eq a => (Board a) -> Bool
isExpandable board = or t where
                t = map f board
                f x = case x of 
                    (Possible _) -> True
                    _ -> False

--FIX !!!!!!!!!!!1


fixedPoint :: Eq a => (a ->a) -> (a -> a)
fixedPoint f board = go f board (f board) where
                        go f x y 
                                | x == y = x
                                | otherwise = go f y (f y)



pfinal :: (Ord a,Eq a) => [Board a] -> [Board a]
pfinal [] = []
pfinal (b:bs) = (let t = (fixedPoint prune b) in if (check t) then (if (isExpandable t) then expandPossible t else [t]) else []) ++ (pfinal bs)

hardBoard = [Possible [1..dim2], Fixed 1, Possible [1..dim2],Fixed 2, Possible [1..dim2],Fixed 3,Possible [1..dim2],Fixed 4,Possible [1..5]]

hardBoard1 = take dim $ repeat (Possible [1..dim2])

--main =  mapM_ (putStrLn) $ map myShow $ fixedPoint pfinal [hardBoard1]

myParse :: [String] -> (Board Int)
myParse [] = []
myParse ("_":xs) = (Possible [1..dim2] ) : (myParse xs)
myParse (x:xs) = Fixed ((read::String->Int) x)  : myParse (xs)

tester :: Board Int
tester = map (Fixed) [1..dim4]


rep n f = foldr (\f g -> f.g) (\x -> x) $ take n (repeat f)


main = do 
    inp <- mapM (\x -> getLine) [1..dim2] 
    let genBoard = myParse $ concat $ map words inp
    voir $ head $ (fixedPoint pfinal)  [genBoard]
