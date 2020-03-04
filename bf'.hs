{-# LANGUAGE FlexibleContexts #-}

import Data.List 
import System.Cmd
import Text.ParserCombinators.Parsec
import Data.Char


data Expr a = Empty | Bracketed (Expr a) (Expr a) | Code a (Expr a) deriving(Show,Eq)

instance Functor Expr where
     fmap _ Empty = Empty
     fmap f (Bracketed first second) = (Bracketed (fmap f first) (fmap f second))
     fmap f (Code code expr) = Code (f code) (fmap f expr)


verify' = do
            head <- (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "")
            case head of
                  "[" -> do {contents <- verify'; string "]"; rest <- verify'; return $ head ++ contents ++ "]" ++ rest}
                  "" -> return []
                  xs -> do {rest <- verify'; return $ xs ++ rest}

-- Verify checks that the code is syntactically correct (ie the brackets match)

verify = do 
            code <- verify'
            rest <- many  $ oneOf "><+-.,[]"
            case rest of 
                  "" -> return code
                  _ -> fail "Unmatched bracket"

check :: String -> Either ParseError String
check str = parse verify "" str


transform = do
            head <- (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "") 
            case head of
                  "[" -> do {contents <- transform; string "]"; rest <- transform; return $ Bracketed contents rest}
                  "" -> return Empty
                  xs -> do {rest <- transform; return $ Code xs rest}


toExpr :: String -> Either ParseError (Expr String)
toExpr str = parse transform "" str


reconstruct :: (Expr String) -> String
reconstruct Empty = ""
reconstruct (Bracketed expr1 expr2) = "[" ++ (reconstruct expr1) ++ "]" ++ (reconstruct expr2)
reconstruct (Code code expr) = code ++ (reconstruct expr)


-- collect epressions of form [(no brackets)]
collect :: (Expr String) -> [String]
collect Empty = []
collect (Bracketed (Code str Empty) expr) = (str) : (collect expr)
collect (Bracketed expr1 expr2) = (collect expr1) ++ (collect expr2)
collect (Code _ expr) = (collect expr)


myOrd :: Ord b => (a,b) -> (a,b) -> Ordering
myOrd (_,x) (_,y) = case (compare x y) of 
                        GT -> LT 
                        EQ -> EQ
                        LT -> GT


-- leafMap only optimizes code of form '[' no bracket ']'
leafMap :: (String -> String) -> (Expr String) -> (Expr String)
leafMap f Empty = Empty
leafMap f (Bracketed (Code str Empty) expr) = Bracketed (Code (f str) Empty) (leafMap f expr)
leafMap f (Bracketed expr1 expr2) = Bracketed (leafMap f expr1) (leafMap f expr2)
leafMap f (Code code expr) = Code code (leafMap f expr)


-- the complement of leafMap
innerMap :: (String -> String) -> (Expr String) -> (Expr String)
innerMap f Empty = Empty
innerMap f (Bracketed (Code str Empty) expr) = Bracketed (Code str Empty) (innerMap f expr)
innerMap f (Bracketed expr1 expr2) = Bracketed (innerMap f expr1) (innerMap f expr2)
innerMap f (Code code expr) = Code (f code) (innerMap f expr)


-- optimize code if possible otherwise compress in case of parse failure
optimize :: String -> String
optimize str = case (parse optimizations "" str) of 
                    (Left _) -> str
                    (Right x) -> x

-- the optimizations convert loops to single instructions requiring the tree to me modified
correct :: (Expr String) -> (Expr String)
correct Empty = Empty
correct (Code code expr) = case (correct expr) of 
                                        (Code code' expr') -> Code (code ++ code') expr'
                                        x -> Code code x
correct (Bracketed (Code str Empty) expr) = case ((head str) `elem` ['f','b','Z']) of
                                                    False -> (Bracketed (Code str Empty) (correct expr))
                                                    True -> case (correct expr) of 
                                                                    (Code str' expr') -> Code (str ++ str') expr'
                                                                    x -> Code str x

correct (Bracketed expr1 expr2) = Bracketed (correct expr1) (correct expr2)


-- applies function on input till convergence
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = go f (f x) x where
                    go f x y = if (x == y) then x else go f (f x) x





append :: (Expr a) -> (Expr a) -> (Expr a)
append x Empty = x
append x (Code code expr) = Code  code (append x expr)
append x (Bracketed y expr) = Bracketed y (append x expr)

-- if you want a new optimization, write a parser for the structure and add it to the list
optimizations = foldl1 (<|>) $ map try [shiftContents,shiftContents',zero]

zero = do 
            string "-"
            return "Z"


shiftContents = do 
                  right <- many1 $ char '>'
                  char '+'
                  left <- many1 $ char '<'
                  char '-'

                  case (length left == length right) of 
                        True -> return ("f" ++ (show $ (length left)))
                        _ -> (fail "Not whats expected")

shiftContents' = do 
                  right <- many1 $ char '<'
                  char '+'
                  left <- many1 $ char '>'
                  char '-'

                  case (length left == length right) of 
                        True -> return ("b" ++ (show $ (length left)))
                        _ -> (fail "Not whats expected")


f :: String -> Either ParseError String
f x = parse shiftContents "" x


($||$) :: (a -> Bool) -> (a -> Bool) -> (a->Bool)
f $||$ g = (\x -> (f x) || (g x))

test parser str = case (parse parser "" str) of 
                    (Left _) -> False
                    _ -> True


compile :: (Expr String -> Expr String)
compile expr =  fixedPoint (\z -> correct $ leafMap optimize z) expr


compress :: String -> String
compress str = foldl1 (++) $ map (\x -> show (head x, length x) ) $ groupBy (==) str

main = do
            code <-  fmap (filter (`elem` "><+-.,[]")) $ readFile "test.bf"
            case (check code) of 
                  (Left x) -> print x
                  (Right code'') -> do 
                                    let transformed = compile $ (\(Right x) -> x) $ toExpr code''
                                    let t' = transformed
                                    print $  transformed
                                    let list =  take 100 $ sortBy myOrd $ map (\x -> (head x,length x)) $ groupBy (==) $  sort $ collect t'
                                    putStrLn "(Potential optimizations,occurence)"
                                    --mapM print $ filter (\x ->  not $ (test optimizations) (fst x)) list
                                    return()