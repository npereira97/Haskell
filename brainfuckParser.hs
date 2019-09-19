import Data.List (group)
import System.Cmd
import Text.ParserCombinators.Parsec
import Control.Monad


verify' = do
            head <- (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "")
            case head of
                  "[" -> do {contents <- verify'; string "]"; rest <- verify'; return $ head ++ contents ++ "]" ++ rest}
                  "" -> return []
                  xs -> do {rest <- verify'; return $ xs ++ rest}


verify = do 
            code <- verify'
            rest <- many $ oneOf "><+-.,[]"
            case rest of 
                  "" -> return code
                  _ -> fail "Unmatched bracket"



check :: String -> Either ParseError String
check str = parse verify "" str


type Code = String
type Instruction = String



start :: Code -> [(Char,Instruction)]
start code = map (\x -> (x,""))  (code)


tag :: Int -> [(Char,Instruction)] -> [(Char,Instruction)]
tag n [] = []
tag n (('[',_):xs) = ('[',"\nlabel"++ (show n) ++ ":" ) : (tag (n+1) xs)
tag n (x:xs) = x : (tag n xs)

tagClose :: [(Char,Instruction)] -> [Instruction]  ->  [(Char,Instruction)]
tagClose [] [] = []
tagClose [] xs = [('!',"Error")]
tagClose (('[',x):xs) stack = ('[',x++"\n\tif (*memory == 0) { goto " ++ "label" ++ (init $ drop 6 x) ++ "z;" ++ "}" ) : (tagClose xs (x:stack))
tagClose ((']',_):xs) (label:stack) =  (']',"\n\tgoto" ++ " " ++ (init $ tail label) ++ ";" ++ "\n"  ++ (init $ tail label) ++ "z" ++ ":" ) : (tagClose xs stack)
tagClose (x:xs) (stack) = x : (tagClose xs stack)

translate :: [(Char,Instruction)] -> [(Char,Instruction)]
translate [] = []
translate (('>',_) :xs ) = ('>',moveForwrd) : translate xs
translate (('<',_) :xs ) = ('<',moveBack) : translate xs
translate (('+',_) :xs ) = ('+',increment) : translate xs
translate (('-',_) :xs ) = ('-',decrement) : translate xs
translate (('.',_) :xs ) = ('.',dot) : translate xs
translate ((',',_) :xs ) = (',',comma) : translate xs
translate (x:xs) = x : translate xs

header = "#include <stdio.h> \n#include <stdlib.h>"


main' = "\n\nint main(void) { \n\n\tint* memory = (int*) malloc(sizeof(int)*100000);"

end = "\n\t return 0; }"

moveForwrd = "\n\tmemory++;"
moveBack = "\n\tmemory--;"

increment = "\n\tmemory[0]++;"
decrement = "\n\tmemory[0]--;"


dot = "\n\tprintf(\"%c\",(char) (*memory));"
comma = "\n\t printf(\"Enter char: \");\n\t*memory = getchar();"


optimize :: [Instruction] -> [Instruction]


optimize lst@(x:xs) = case x of 
                        "\n\tmemory++;" -> return $ "\n\tmemory = memory + " ++ (show (length lst)) ++ ";" 
                        "\n\tmemory--;" -> return $ "\n\tmemory = memory - " ++ (show (length lst)) ++ ";" 
                        "\n\tmemory[0]++;" -> return $ "\n\tmemory[0] = memory[0] + " ++ (show (length lst)) ++ ";" 
                        "\n\tmemory[0]--;" -> return $ "\n\tmemory[0] = memory[0] - " ++ (show (length lst)) ++ ";" 
                        otherwise -> lst where
                            l = length lst


main = do
            code <-  liftM (filter (`elem` "><+-.,[]")) $ readFile "test.bf"
            case (check code) of 
                  (Left x) -> print x
                  (Right code') -> do 
                                    putStrLn "Ready to compile"
                                    let xs = concat $ map optimize $ group $ map snd $ translate $ tagClose (tag 0 (start code)) []
                                    --let xs =  map snd $ translate $ tagClose (tag 0 (start str)) []
                                    writeFile "test.c" $ header ++ main' ++ (concat xs) ++ end
                                    system "gcc test.c"
                                    putStrLn "Compilation complete"
                                    system "./a.out"
                                    return()
