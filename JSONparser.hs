{-# LANGUAGE FlexibleContexts #-}

import Text.ParserCombinators.Parsec
import Data.Char
import Text.Read



quotedString = do 
                many $ string " "
                string "\""
                str <- many $ noneOf "\""
                string "\""
                many $ string " "
                return str



parseFloat = do
                many $ string " "
                minus <- string "-" <|> string ""
                many $ string " "
                n <- do {head <- many1 (satisfy isDigit); string ".";tail<- many1 (satisfy isDigit); return(head ++ "." ++ tail)}
                many $ string " "
                case minus of
                    "-" -> return $ "-" ++ n
                    _ -> return n


parseInt = do
                many $ string " "
                minus <- string "-" <|> string ""
                many $ string " "
                n <- many1 (satisfy isDigit)
                many $ string " "
                case minus of
                    "-" -> return $ "-" ++ n
                    _ -> return n
                
                


parseVal = do 
            many $ string " "
            value <- (string "Null" ) <|> (string "true") <|> (string "false") <|> (try parseFloat) <|> (parseInt) <|> quotedString <|> (string "{") <|> (string "[")
            case value of 
                "[" -> do {valArr <- sepBy parseVal (string ","); string "]" ;return (Array valArr) }
                "{" -> do  {val <- parseJSON'; return (JSON' val)}
                "true" -> return (Boolean True)
                "false" -> return (Boolean False)
                "Null" -> return Null
                str -> case ((readMaybe str) :: Maybe Double) of
                        Just x -> if '.' `elem` str then return (Double x) else return $ Integer (truncate x)
                        Nothing -> return (Str str)


parseArray' = do
            many $ string " "
            end <- string "]" <|> string "," <|> string ""
            case end of 
                "]" -> return []
                _ -> do {val <- parseVal; rest <- parseArray'; return (val:rest) }


parseArray'' itemParser = do
                    many $ string " "
                    end <- string "]" <|> string "," <|> string ""
                    case end of 
                        "]" -> return []
                        _ -> do {val <- itemParser; rest <- (parseArray'' itemParser); return (val:rest) }

parseArray = do 
            many $ string " "
            string "["
            json <- (parseArray'' parseJSON)
            return json

parseJSON' =  do 
                many (string " ")
                m <- ( (string "}") <|> (string ",") <|> string "")
                case m of "}" -> return []
                          _ -> do {
                            key <- quotedString;
                            string ":";
                            many (string " ");
                            value <- parseVal;
                            restJSON <- parseJSON' ;
                            return $ ((Entry key value) : restJSON)
                        }
                
parseJSON = do 
            many (string " ")
            string "{"
            xs <- parseJSON'
            return xs
                

type JSON = [Entry] 
data Val = Str String | Integer Int | Double Double | Boolean Bool | Array [Val] | JSON' JSON | Null deriving (Eq)
data Entry = Entry String Val deriving (Eq)

mid (x:y:[]) = x:y:[] 
mid (x:y:xs) = y : (init xs)

f '[' = '{'
f ']' = '}'
f x = x


g xs = "{" ++ (mid xs) ++ "}"

instance Show Val where
    show (Str x) = show x
    show (Integer x) = show x
    show (Double x) = show x
    show (Boolean x) = show x
    show (JSON' x) = map f $ show x
    show (Array x) = show x
    show (Null) = "Null"




instance Show Entry where
    show (Entry str x) = (show str) ++ ":" ++ (show x)
     

par :: String -> Either ParseError String
par str = parse quotedString "" str

par1 :: String -> Either ParseError JSON
par1 str = parse parseJSON "" str

 
par2 :: String -> Either ParseError [JSON]
par2 str = parse parseArray "" str


x = do
        y <- getLine
        return y


enter = x >>= (\z -> return $ par1 z) 


enter' = do 
            l <- getLine
            str <- return $ par2 l
            temp <- return $ (\(Right x) -> x) str
            putStrLn $ g $ show temp

            return()



