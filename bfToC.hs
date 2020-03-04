{-# LANGUAGE FlexibleContexts #-}

import Data.List 
import System.Cmd
import Text.ParserCombinators.Parsec
import Data.Char




data Code a = Empty | Bracketed (Code a) (Code a) | Code a (Code a) deriving(Show,Eq)

instance Functor Code where
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


toCode :: String -> Code String
toCode str = case (parse transform "" str) of
                        (Right x) -> x
                        (Left _) -> Empty


reconstruct :: (Code String) -> String
reconstruct Empty = ""
reconstruct (Bracketed expr1 expr2) = "[" ++ (reconstruct expr1) ++ "]" ++ (reconstruct expr2)
reconstruct (Code code expr) = code ++ (reconstruct expr)


-- collect epressions of form [(no brackets)]
collect :: (Code String) -> [String]
collect Empty = []

collect (Bracketed (Code str Empty) expr) = (str) : (collect expr)
collect (Bracketed expr1 expr2) = (collect expr1) ++ (collect expr2)
collect (Code _ expr) = (collect expr)


myOrd :: Ord b => (a,b) -> (a,b) -> Ordering
myOrd (_,x) (_,y) = compare y x


-- leafMap only optimizes code of form '[' no bracket ']'
leafMap :: (String -> String) -> (Code String) -> (Code String)
leafMap f Empty = Empty
leafMap f (Bracketed (Code str Empty) expr) = Bracketed (Code (f str) Empty) (leafMap f expr)
leafMap f (Bracketed expr1 expr2) = Bracketed (leafMap f expr1) (leafMap f expr2)
leafMap f (Code code expr) = Code code (leafMap f expr)


-- the complement of leafMap
innerMap :: (String -> String) -> (Code String) -> (Code String)
innerMap f Empty = Empty
innerMap f (Bracketed (Code str Empty) expr) = Bracketed (Code str Empty) (innerMap f expr)
innerMap f (Bracketed expr1 expr2) = Bracketed (innerMap f expr1) (innerMap f expr2)
innerMap f (Code code expr) = Code (f code) (innerMap f expr)


-- optimize code if possible otherwise compress in case of parse failure
optimize :: String -> String
optimize str = case (parse optimizations' "" str) of 
                    (Left _) -> str
                    (Right x) -> x


-- the optimizations convert loops to single instructions requiring the tree to me modified
correct :: (Code String) -> (Code String)
correct Empty = Empty
correct (Code str code1) = undefined

correct (Bracketed (Code str Empty) next ) =  case (parse correcter "" str) of
                                                    (Left _) -> Bracketed (Code str Empty) (correct next) 
                                                    (Right x) -> Code (str ++ x) (correct next)
correct (Bracketed code1 code2) = Bracketed (correct code1) (correct code2)
-- applies function on input till convergence
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f x = go f (f x) x where
                    go f x y = if (x == y) then x else go f (f x) x






append :: (Code a) -> (Code a) -> (Code a)
append x Empty = x
append x (Code code expr) = Code code (append x expr)
append x (Bracketed y expr) = Bracketed y (append x expr)



optimizations' = do
                    tok <- optimizations
                    rest <- many (satisfy (\_ -> True))
                    case rest of 
                        "" -> return tok
                        _ -> fail "Blah"

-- if you want a new optimization, write a parser for the structure and add it to the list
optimizations = foldl1 (<|>) $ map try [zero,nextOne,shiftContents,shiftContents']

-- Left is exceptional
opt' :: Code String -> Code String
opt' Empty = Empty
opt' (Code str code') = Code str (opt' code')

-- Code str (opt' code')
opt' (Bracketed (Code str Empty) code'') = let str' = (optimize str) in 
                                                case (str' /= str) of
                                                    True -> Code str' (opt' code'')
                                                    False -> Bracketed (Code str Empty) (opt' code'')
opt' (Bracketed code code') = Bracketed (opt' code) (opt' code')

correcter = do
                first <- optimizations
                rest <- many (satisfy (\x -> True))
                case rest of 
                    "" -> return first
                    _ -> fail "More than one expression"



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

nextOne = do 
            string ">Z+<-"
            return "o"


f :: String -> Either ParseError String
f x = parse shiftContents "" x


($||$) :: (a -> Bool) -> (a -> Bool) -> (a->Bool)
f $||$ g = (\x -> (f x) || (g x))

test parser str = case (parse parser "" str) of 
                    (Left _) -> False
                    _ -> True



compress :: String -> String
compress str = foldl1 (++) $ map (\x -> show (head x, length x) ) $ groupBy (==) str

compress' :: String -> String
compress' str =  map (\x -> head x ) $ groupBy (==) str


stick :: Code String -> Code String
stick (Code str code') = let code'' = stick code' in 
                            case (code'') of 
                                (Code str1 code1) -> Code (str++str1) code1
                                _ -> Code str code''
stick Empty = Empty
stick (Bracketed x x') = Bracketed (stick x) (stick x')



translate :: (Code String) -> Int -> (Int,String) 
translate Empty n = (n,"")
translate (Code code next) n = let (n',code') = translate (next) n in (n',code ++ code')
translate (Bracketed code next) n = let (n',code') = translate code (n+1) in let (n'',code'') = (translate next n') in (n'',(loop n) ++code'  ++ (loopEnd n) ++ code'')


translate' :: Code String -> String
translate' Empty  = ""
translate' (Code code next)  = let code' = translate' (next)  in (code ++ code')
translate' (Bracketed code next)  = let code' = translate' code in let code'' = translate' next in ("\n\n\twhile(*memory != 0){\n" ++code'  ++ "\n}"++ code'')



multiParser = foldl1 (<|>) $ map (many1.char) "<>+-"
singleParser = foldl1 (<|>) $ map string $ map (:[]) ".,oZ"
numberTerminatedParser = foldl1 (<|>) $ map f $ map (:[]) "fb" where 
                                        f x = do
                                                first <- (string x)
                                                n <- (many1 (satisfy isDigit))
                                                return $ first ++ n

shiftParser = shiftContents <|> shiftContents'
codeParser = foldl1 (<|>) [(try shiftParser),multiParser,singleParser,numberTerminatedParser,string ""]


codeParser' = foldl1 (<|>) [multiParser,singleParser,numberTerminatedParser,string ""]


-- use this when trying to
codeToInstruction = do 
                        first <- codeParser
                        case first of 
                            "" -> return []
                            _ ->  do
                                    rest <- codeToInstruction
                                    case first of
                                        "o" -> return $ "\n\tIncNext(memory);" ++ rest
                                        "Z" -> return $ "\n\tzeroOut(memory);" ++ rest
                                        ('f':n) -> return $ (shift (read n)) ++ rest
                                        ('b':n) -> return $ (shift (-(read n))) ++ rest
                                        ('+':_) -> return $ "\n\tmemory[0] = memory[0] + " ++ (show (length first)) ++";" ++ rest
                                        ('-':_) -> return $ "\n\tmemory[0] = memory[0] - " ++ (show (length first)) ++";" ++ rest  
                                        ('>':_) -> return $ "\n\tmemory = memory + " ++ (show (length first)) ++";" ++ rest
                                        ('<':_) -> return $ "\n\tmemory = memory - " ++ (show (length first)) ++";" ++ rest  
                                        ('.':_) -> return $ dot ++ rest
                                        (',':_) -> return $ comma ++ rest






compressCode :: String -> String
compressCode str = case (parse codeToInstruction "" str) of
                        (Left _) -> str
                        (Right x) -> x




-- Use this with inner map
codeToInstruction' = do 
                        first <- codeParser'
                        case first of 
                            "" -> return []
                            _ ->  do
                                    rest <- codeToInstruction
                                    case first of
                                        "o" -> return $ "\n\tIncNext(memory);" ++ rest
                                        "Z" -> return $ "\n\tzeroOut(memory);" ++ rest
                                        ('f':n) -> return $ (shift (read n)) ++ rest
                                        ('b':n) -> return $ (shift (-(read n))) ++ rest
                                        ('+':_) -> return $ "\n\tmemory[0] = memory[0] + " ++ (show (length first)) ++";" ++ rest
                                        ('-':_) -> return $ "\n\tmemory[0] = memory[0] - " ++ (show (length first)) ++";" ++ rest  
                                        ('>':_) -> return $ "\n\tmemory = memory + " ++ (show (length first)) ++";" ++ rest
                                        ('<':_) -> return $ "\n\tmemory = memory - " ++ (show (length first)) ++";" ++ rest  
                                        ('.':_) -> return $ dot ++ rest
                                        (',':_) -> return $ comma ++ rest



compressCode' :: String -> String
compressCode' str = case (parse codeToInstruction' "" str) of
                        (Left _) -> str
                        (Right x) -> x



loop :: Int -> String
loop n = "\nlabel" ++ (show n) ++ ":" ++ "\n\tif (*memory == 0) { goto " ++ "label" ++ (show n) ++ "z;}" 

loopEnd :: Int -> String
loopEnd n = "\n\tgoto label" ++ (show n) ++ ";" ++ "\nlabel" ++ (show n) ++ "z:"


header = "#include <stdio.h> \n#include <stdlib.h>"

shift :: Int -> String
shift n = "\n\tshift(memory," ++ (show n) ++ ");"

shiftFun = "\nvoid shift(int* arr, int n) {\n\t\n\tarr[n] += arr[0]; \n\tarr[0] = 0;\n}"
zeroOutFun = "\nvoid zeroOut(int* arr) {\n\t\n\tarr[0] = 0;\n}"
incNextFun = "\nvoid IncNext(int* arr) {\n\t\n\tarr[0] = 0; arr[1] = 1;\n}"

functions = foldl1 (++) [shiftFun,zeroOutFun,incNextFun]


main' = "\n\nint main(void) { \n\n\tint* memory = (int*) malloc(sizeof(int)*100000);"

end = "\n\t return 0; }\n"

dot = "\n\tprintf(\"%c\",(char) (*memory));"
comma = "\n\tprintf(\"Enter char: \");\n\t*memory = getchar();"



codeZip :: (Code a) -> (Code b) -> [(a,b)]
codeZip Empty Empty = []
codeZip (Code code expr) (Code code' expr') = (code,code') : (codeZip expr expr')
codeZip (Bracketed code1 code2) (Bracketed code1' code2') = (codeZip code1 code1') ++ (codeZip code2 code2') 

final tree =  header ++ functions ++ main' ++ (snd $ translate (innerMap compressCode' $ leafMap compressCode tree) 0) ++ end

final' tree =  header ++ functions ++ main' ++ ( translate' (innerMap compressCode' $ leafMap compressCode tree) ) ++ end


compile :: (Code String -> Code String)
compile expr =  fixedPoint (stick.opt') expr

main = do
            code <-  fmap (filter (`elem` "><+-.,[]")) $ readFile "test.bf"
            case (check code) of 
                  (Left x) -> print x
                  (Right code'') -> do 
                                    --print code''
                                    let transformed = toCode code''
                                    let tree =  transformed :: Code String-- This is the Code representation of the code
                                    --print $ fmap (compressCode) tree
                                    --print $ collect t'
                                    --print t'
                                    --print $ transformed
                                    let tree =  compile transformed
                                    --print tree
                                    --putStrLn $ final' tree
                                    --putStrLn $ final' tree
                                    --mapM_ print $ codeZip t' (fmap compressCode t')
                                    --putStrLn $ final t'
                                    --print transformed
                                    --putStrLn "(Potential optimizations,occurence)"
                                    let collected = collect tree
                                    print $ length collected
                                    let list =  sortBy myOrd $ map (\x -> (head x,length x)) $ groupBy (==) $  sort $ collected
                                    --mapM print $ filter (\x ->  not $ (test optimizations) (fst x)) $ take 100 list
                                    mapM print $ take 100 list

                                    return()
