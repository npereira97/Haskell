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
                n <- do {head <- many1 (satisfy isDigit); string ".";tail<- many1 (satisfy isDigit); return(head ++ "." ++ tail)}
                many $ string " "
                return n


parseVal = do 
            many $ string " "
            value <- (string "true") <|> (string "false") <|> (try parseFloat) <|> (many1 (satisfy isDigit)) <|> quotedString <|> (string "{") <|> (string "[")
            case value of 
                "[" -> do {valArr <- parseArray'; return (Array valArr) }
                "{" -> do  {val <- parseJSON'; return (JSON' val)}
                "true" -> return (Boolean True)
                "false" -> return (Boolean False)
                str -> case ((readMaybe str) :: Maybe Double) of
                        Just x -> return (Double x)
                        Nothing -> case ((readMaybe str) :: Maybe Int) of
                                        Just x -> return (Integer x)
                                        Nothing -> return (Str str)



parseArray' = do
            many $ string " "
            end <- string "]" <|> string "," <|> string ""
            case end of 
                "]" -> return []
                _ -> do {val <- parseVal; rest <- parseArray'; return (val:rest) }




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
data Val = Str String | Integer Int | Double Double | Boolean Bool | Array [Val] | JSON' JSON deriving (Eq)
data Entry = Entry String Val deriving (Eq)


instance Show Val where
    show (Str x) = show x
    show (Integer x) = show x
    show (Double x) = show x
    show (Boolean x) = show x
    show (JSON' x) = show x
    show (Array x) = show x



instance Show Entry where
    show (Entry str x) = (show str) ++ ":" ++ (show x)
     

par :: String -> Either ParseError String
par str = parse quotedString "" str

par1 :: String -> Either ParseError JSON
par1 str = parse parseJSON "" str


x = do
        y <- getLine
        return y


enter = x >>= (\z -> return $ par1 z)