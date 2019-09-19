import Text.ParserCombinators.Parsec
import Data.Char


postFix = do
			first <- many1 (satisfy (isDigit)) <|> 