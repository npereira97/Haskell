import System.Random
import Data.List hiding(insert)

insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs) = if (e < x) then (e:x:xs) else (x:(insert e xs))

data HTree = Node String Double | Tree String (HTree) (HTree) Double deriving(Show)

getVal :: HTree-> Double
getVal (Node _ x) = x
getVal (Tree _ _ _ x) = x


getName :: HTree -> String
getName (Node x _) = x
getName (Tree x _ _ _) = x


instance Eq (HTree) where
	(==) c d = (getVal c) == (getVal d)

instance Ord (HTree) where
	compare c d = compare (getVal c) (getVal d)


merge :: HTree -> HTree -> HTree
merge c d  = Tree (getName(c) ++ getName(d)) c d (getVal(c) + getVal(d))


buildTree :: [HTree] -> HTree
buildTree (x:[]) = x
buildTree (x:y:xs) = buildTree $ insert (merge x y) xs


getCodes :: HTree -> [(String,String)]
getCodes (Node str _) = return $ (str,"")
getCodes (Tree str left right _) = ((map.fmap) (\x -> "1"++x) (getCodes left)) ++ ((map.fmap) (\x -> "0"++x) (getCodes right)) 


randomTree :: IO HTree
randomTree = do 
				xs <- sequence [randomRIO (0,10000) | _ <- [1..1000]]
				let xs' = sort $ map (uncurry Node) $ zip (map (:[]) ['A'..]) xs
				let tree =  buildTree xs'
				return tree

main = do 
		tree <- randomTree
		--print tree
		mapM_ print $ getCodes tree

