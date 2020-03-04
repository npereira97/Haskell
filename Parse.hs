module Parse where

import Data.Char

newtype Parser a = Parser (String -> [(a,String)])


parse :: Parser a -> String -> [(a,String)]
parse (Parser p) str = p str


failure :: Parser a
failure = Parser (\x -> [])

item :: Parser Char
item = Parser p where
            p [] = []
            p (x:xs) = [(x,xs)]

instance Functor Parser where
    fmap f (Parser p) = Parser (\str -> map m (p str)) where
                            m (x,y) = (f x,y)


instance Applicative Parser where
    pure v = Parser (\x -> [(v,x)])
    (<*>) (Parser f) (Parser x) = undefined


instance Monad Parser where
    return = pure
    (>>=) (Parser p) (f) = Parser (\x -> case (p x) of 
                                                    [] -> []
                                                    [(v,vs)] -> parse (f v) vs)


sat :: (Char -> Bool) -> Parser Char
sat pred = do
                c <- item
                if (pred c) then return c else failure


(+++) :: Parser a -> Parser a -> Parser a


(+++) p1 p2 = Parser (\x -> case (parse p1 x) of 
                                    [] -> (parse p2 x)
                                    [(v,vs)] ->  [(v,vs)] )


char:: Char -> Parser Char
char c = sat (==c)



sat' :: (Char -> Bool) -> Parser String
sat' pred = do
                c <- item
                if (pred c) then return [c] else failure

char' :: Char -> Parser String
char' c = sat' (==c)


string :: String -> Parser String
string [] = return []
string (x:xs) = do
                (char x)
                (string xs)
                return (x:xs)

many :: Parser a -> Parser [a]
many p = (many1 p) +++ (return [])

many1 :: Parser a -> Parser [a]
many1 p = do 
            v <- p
            vs <- many p
            return (v:vs)



parseN :: Int -> Parser a -> Parser [a]
parseN 1 p = do 
                x <- p
                return ([x]) 
parseN n p = do 
                x <- p
                xs <- (parseN (n-1) p)
                return (x:xs)


token :: Parser String
token = do
        _ <- many (sat isSpace)
        xs <- (many ((sat isDigit) +++ (sat isAlpha)) )
        _ <-  many (sat isSpace)
        return xs


p :: Parser Int
p = do
        many (sat isSpace)
        n <- (many1 (sat isDigit))
        many (sat isSpace)
        return (read n)


p' :: Parser [Int]
p' = do
    n <- p
    t <- (char ',') +++ (char ']')
    case t of 
        ']' -> return [n]
        ',' -> do { ns <- p' ;  return (n:ns) }

p'' :: Parser [Int]
p'' = do
        many (sat isSpace)
        return []

format :: Parser [Int]
format = do
            many (sat isSpace)
            (char '[')
            ns <- (p' +++ p'')
            return ns




