import Parse
import Data.Char

data Op = Add | Sub | Mul | Div | Exp deriving(Eq,Show,Ord)

data ParseTree a = Leaf a | Internal Op (ParseTree a) (ParseTree a) | Empty | Lock (ParseTree a) deriving (Eq,Show)

z = 1 + 2 * 3 - 4 
t = 2*4 + 3


instance Functor ParseTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Internal op t1 t2) = Internal op (fmap f t1) (fmap f t2)
    fmap _ Empty = Empty
    fmap f (Lock t) = Lock (fmap f t) 


myCompare :: Op -> Op -> Ordering
myCompare Add Sub = EQ
myCompare Sub Add = EQ

myCompare Mul Div = EQ
myCompare Div Mul = EQ

myCompare x y = compare x y

toOp :: String -> Op
toOp "+" = Add
toOp "-" = Sub
toOp "*" = Mul
toOp "/" = Div
toOp "^" = Exp


insert :: (Num a,Read a) => [String] -> (ParseTree a) -> (ParseTree a)
insert [] t = t

insert expr@("(":xs) Empty = let (first,rest) = sep expr in insert (rest) $ Lock (insert first Empty) 
insert expr@(o:"(":xs) (Leaf y) = let (first,rest) = sep (tail expr) in insert (rest) $ Internal (toOp o) (Leaf y) $ Lock (insert first Empty) 

insert expr@(o:"(":xs) tree@(Internal oper t1 t2) = let (first,rest) = sep (tail expr) in
                                                                case (myCompare oper (toOp o) ) of 
                                                                    EQ -> insert rest $ Internal (toOp o)  tree $ Lock (insert first Empty)
                                                                    GT -> insert rest $ Internal (toOp o)  tree  $ Lock (insert first Empty)
                                                                    LT -> insert rest  $ Internal oper t1 (Internal (toOp o)   t2  $ Lock (insert first Empty))

insert expr@(o:"(":xs) (Lock tree) = let (first,rest) = sep (tail expr) in Lock $ Internal (toOp o) (Lock tree) (insert first Empty)



insert (x:xs) Empty = insert xs $ Leaf (read x)

insert (op:x:xs) (Leaf y) = insert xs $ Internal (toOp op) (Leaf y) (Leaf (read x))
insert (op:x:xs) (Lock tree) = insert xs $ Internal (toOp op)  (Lock tree) (Leaf (read x))
insert (op:x:xs) tree@(Internal oper t1 t2) = case (myCompare oper (toOp op) ) of 
                                                    EQ -> insert xs $ Internal (toOp op)  tree (Leaf (read x))
                                                    GT -> insert xs $ Internal (toOp op)  tree (Leaf (read x))
                                                    LT -> insert xs $ Internal oper t1 (Internal (toOp op)  t2 (Leaf (read x)))


sep :: [String] -> ([String],[String])
sep ("(":xs) = go ([],xs) (-1) where
            go (xs,ys) 0 = (init xs,ys)
            go (xs,"(":ys) n = go (xs++["("],ys) (n-1)
            go (xs,")":ys) n = go (xs++[")"],ys) (n+1)
            go (xs,y:ys) n = go (xs ++ [y],ys) n


eval ::  (ParseTree Double) -> (Double)
eval (Leaf x) = x
eval (Internal op left right) = case op of 
                            Add -> (eval left) + (eval right)
                            Sub -> (eval left) - (eval right)
                            Mul -> (eval left) * (eval right)
                            Div -> (eval left) / (eval right)
                            Exp -> (eval left) ** (eval right)
eval (Lock tree) = eval tree


eval Empty = 0



parseExpr' :: Parser String
parseExpr' = do
            many (sat isSpace)
            t <- (many1 (sat isDigit)) +++ ((char') '+') +++ ((char') '-') +++ ((char') '*') +++ ((char') '/')+++ ((char') '^') +++ ((char') '(') +++ ((char') ')')
            many (sat isSpace)
            return t

consm str parser = case (parse parser str) of 
                        [] -> []
                        [(x,xs)] -> x: (consm xs parser)



toList :: String -> [String]
toList str = consm str parseExpr'

buildTree expr =  insert (consm expr parseExpr') Empty

calc ::  String -> Double
calc expr = eval $ buildTree expr

run :: String -> IO()
run str = do
            tree <- return (buildTree str) :: (IO (ParseTree Double))
            putStrLn $ show $ tree
            let n = eval tree
            print n
            return()