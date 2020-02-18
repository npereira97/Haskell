import Text.ParserCombinators.Parsec



nested :: Parser a -> Parser a
nested parser = do 
                    spaces
                    char '('
                    spaces
                    cont <- (try parser) <|> (nested parser)
                    spaces
                    char ')'
                    return cont
