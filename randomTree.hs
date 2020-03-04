import System.Random
import Data.List

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq,Show)


buildTree ::  IO (Tree Int)

buildTree = do 
                x <- (randomRIO (0,1000)) :: IO Int;
                let surviveProb = 500
                case (x > surviveProb) of 
                        True -> return Leaf
                        _ -> do 
                                ltree <- buildTree
                                rtree <- buildTree
                                return  $ Node x ltree rtree
                            
                



height :: Tree a -> Int
height Leaf = 0
height (Node _ ltree rtree) = 1 + max (height ltree) (height rtree)



randomTest = fmap sort $ (fmap.map) (`mod` 2) $ sequenceA [randomRIO (0,1076980) | _ <- [1..20]] :: IO [Int]
average lst = fromIntegral (sum lst) / fromIntegral (length lst)

test = do 
        lst <- sequenceA $ map (\_ -> buildTree) [1..20000]
        lst'<- return $  map height lst 
        --print $ take 200 lst'
        return $ average lst'


test' = do 
            let lst = map (\_ -> buildTree) [1..10]
            lst' <- sequence $  (fmap.fmap) height lst 
            print $ average lst'

           
                



groupTest = mapM (\_ -> test) [1..20]