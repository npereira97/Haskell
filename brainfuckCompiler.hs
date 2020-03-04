{-# LANGUAGE FlexibleContexts #-}

import Data.List (group)
import System.Cmd
import Text.ParserCombinators.Parsec
import Data.Char


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


--substitutes sequences of instructions for optimization purposes
-- zero -> [-]
-- shiftContents -> [>^n+<^n-]
condense = do
            head <- (try zero) <|> (try shiftContents) <|> (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "") 
            case head of
                  "[" -> do {contents <- condense; string "]"; rest <- condense; return $ head ++ contents ++ "]" ++ rest}
                  "" -> return []
                  '*':xs -> do {rest <- condense; return $ xs ++ rest}
                  xs -> do {rest <- condense; return $ xs ++ rest}



-- parses tokens and constructs code
compile n = do 
                  head <- (many1 (satisfy isDigit) ) <|> (string "Z") <|> (string "[")  <|> (string ".") <|> (string ",") <|> (many1 (char '+')) <|> (many1 (char '-')) <|> (many1 (char '>')) <|> (many1 (char '<')) <|>  (string "")
                  case head of 
                        "" -> return (n,"")
                        "Z" -> do {(n',rest) <- (compile n); return (n',zeroOut ++ rest) }
                        "[" -> do {(n',contents) <- (compile (n+1)); string "]"; (n'',rest) <- (compile n'); return (n''+1,(loop n) ++ contents ++ (loopEnd n) ++ rest)}
                        str@(x:xs) -> case (isDigit x) of 
                                                True -> do {(n',rest) <- (compile n); return (n',"\n\tshift(memory,"++(show ((read::String->Int) str)) ++ ");" ++ rest)}
                                                False -> case x of 
                                                                  '+' -> do {(n',rest) <- (compile n); return (n',"\n\tmemory[0] = memory[0] + " ++ (show (length str)) ++";" ++ rest)}
                                                                  '-' -> do {(n',rest) <- (compile n); return (n',"\n\tmemory[0] = memory[0] - " ++ (show (length str)) ++";" ++ rest)}  
                                                                  '>' -> do {(n',rest) <- (compile n); return (n',"\n\tmemory = memory + " ++ (show (length str)) ++";" ++ rest)}
                                                                  '<' -> do {(n',rest) <- (compile n); return (n',"\n\tmemory = memory - " ++ (show (length str)) ++";" ++ rest)}  
                                                                  '.' -> do {(n',rest) <- (compile n); return (n',dot++ rest)}
                                                                  ',' -> do {(n',rest) <- (compile n); return (n',comma ++ rest)}






extract = do 
            first <- (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "")
            case first of 
                "[" -> do {contents <- extract'; string "]"; rest <- extract; return $ contents ++ rest}
                "" -> return []
                xs -> do {rest <- extract; return rest}

extract' = do 
            first <- (string "[") <|> (many1 $ oneOf "><+-.,")  <|> (string "")
            case first of
                "[" -> do {contents <- extract'; string "]"; rest <- extract; return $ contents ++ rest}
                "" -> return []
                xs -> do     
                            rest <- extract
                            let xs' = (if '[' `elem` xs then (g xs) else [xs])
                            return (xs'++rest)




g str = (\(Right x) -> x) $ parse extract "" str


partition :: [a] -> (a -> Bool) -> ([a],[a])
partition [] _ = ([],[])
partition (x:xs) f = let (yes,no) = (partition xs f) in if (f x) then (x:yes,no) else (yes,x:no) 

h [] = []
h x = let temp = (g x) in let (no,yes) = partition temp (\x -> '[' `elem` x ) in yes ++ (h (concat no))




                                

loop :: Int -> String
loop n = "\nlabel" ++ (show n) ++ ":" ++ "\n\tif (*memory == 0) { goto " ++ "label" ++ (show n) ++ "z;}" 

loopEnd :: Int -> String
loopEnd n = "\n\tgoto label" ++ (show n) ++ ";" ++ "\nlabel" ++ (show n) ++ "z:"

type Code = String
type Instruction = String



header = "#include <stdio.h> \n#include <stdlib.h>"

shiftFun = "\nvoid shift(int* arr, int n) {\n\t\n\tarr[n] += arr[0]; \n\tarr[0] = 0;\n}"

main' = "\n\nint main(void) { \n\n\tint* memory = (int*) malloc(sizeof(int)*100000);"

end = "\n\t return 0; }\n"

dot = "\n\tprintf(\"%c\",(char) (*memory));"
comma = "\n\tprintf(\"Enter char: \");\n\t*memory = getchar();"

zeroOut = "\n\tmemory[0] = 0;"



main = do
            code <-  fmap (filter (`elem` "><+-.,[]")) $ readFile "test.bf"
            case (check code) of 
                  (Left x) -> print x
                  (Right code') -> do 
                                    let condensed = (\(Right x) -> x) $ parse condense "" (code')
                                    --putStrLn condensed
                                    let compiled = (\(Right x) -> x) $ parse (compile 0) "" (condensed)
                                    putStrLn $ header ++ shiftFun ++ main' ++ (snd compiled) ++ end

                                    return ()


-- shiftContents -> [>^n+<^n-]
shiftContents = do 
                  char '['
                  right <- many1 $ char '>'
                  char '+'
                  left <- many1 $ char '<'
                  char '-'
                  char ']'

                  case (length left == length right) of 
                        True -> return ("*" ++ (show $ (length left)))
                        _ -> (fail "Not whats expected")


zero = do 
            string "[-]"
            return "Z"


