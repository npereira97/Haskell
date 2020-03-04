data Tree a = Leaf | Tree (Tree a) a (Tree a) deriving (Eq,Show)

children :: Tree a -> [Tree a]
children Leaf = []
children (Tree Leaf x Leaf) = []
children (Tree Leaf x rtree) = [rtree]
children (Tree ltree x Leaf) = [ltree]
children (Tree ltree x rtree) = [ltree,rtree]

build :: Int -> Tree Int
build 0 = Leaf
build n = Tree (build (n+1)) n (build (n+1))

data Queue a = Queue [a] deriving (Show,Eq)

enqueue :: [a] -> Queue a -> Queue a
enqueue xs (Queue q) = Queue (q ++ xs)

proc :: Queue (Tree a) -> [a]
proc (Queue []) = []
proc (Queue (x:xs)) =  (f x) : (proc (enqueue (children x) (Queue xs) ) )

f = (\(Tree _ x _ ) -> x) 