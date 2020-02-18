import Parse
import Data.Char




csv :: Parser [String]
csv = do 
        x <- token
        sep <- (string ",") +++ (string "")
        case sep of 
            "," -> do {xs <- csv; return (x:xs)}
            "" -> return [x]


csvTable :: Parser [[String]]
csvTable = do
            line <- csv
            sep <- ((char') '\n') +++ (string "")
            case sep of 
                "\n" -> do {lines <- csvTable; return (line:lines)}
                "" -> return [line]




f :: String -> String
f [] = []
f (x:xs) = (toUpper x):xs


mail :: Parser String
mail = do
    first <- (many ((sat isDigit) +++ (sat isAlpha)))
    char '.'
    last <- (many ((sat isDigit) +++ (sat isAlpha)))
    string "@mail.mcgill.ca"
    return (filter (\x -> isAlpha x || x == ' ') (f first ++ " " ++ f last))
