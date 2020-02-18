import Text.ParserCombinators.Parsec

rep 1 parser = do 
                x <- parser
                return [x] 
rep n parser = do 
                x <- parser
                xs <- (rep (n-1) parser)
                return (x:xs)


startParser = try (string "AUG") <|> ((satisfy (\_ -> True)) >> startParser)
codonParser = do 
                head <- rep 3 (oneOf "AUGC")
                case (elem head ["UAA","UAG","UGA"]) of 
                    True -> return []
                    False -> do {rest <- codonParser; return (head:rest)}
                


startParser' = do 
                    start <- startParser <|> string ""
                    case start of 
                        "" -> return []
                        _  ->  do
                                rest <- lookAhead $ many (satisfy (\_ -> True)) :: Parser String
                                inf <- finalParser' :: Parser [String]
                                case (parse (startParser') "" rest) of
                                    (Left _ ) -> return [inf]
                                    (Right xss) -> return $ inf : xss




finalParser' = do
                mid <- codonParser
                return ("AUG":mid)

finalParser = do
                start <- startParser
                mid <- codonParser
                return (start:mid)




f 'T' = 'U'
f x = x

main = do 
            str <- readFile "test.txt"
            str' <- return $ (filter (\x -> elem x "AUGC") $ map f str) 
            print $ parse startParser' "" str'
            return()