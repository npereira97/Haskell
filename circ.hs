import System.Random
import Data.List

toSign :: Double -> IO Double
toSign d = do 
			 	n <- randomRIO (0,1) :: IO Int
			 	case n of 
			 		1 -> return d
			 		0 -> return (-d)



randomTuple :: IO ((Double,Double))
randomTuple = do 
				x <- (randomRIO (-1000,1000)) :: IO Double
				y <- toSign ((1-(x/1000))**(0.5)) 

				return (x/1000,y)


randChord :: IO Double
randChord = do 
				p1 <- randomTuple
				p2 <- randomTuple
				return $ distance p1 p2



prob :: IO Double
prob = do
		let n = 10000
		xs <- sequence [randChord | x <- [1..n]]
		let smaller = length $ filter (< 1.732) xs
		let larger = n - smaller
		let p = (fromIntegral smaller)/ (fromIntegral n)
		--print xs
		return p


main = do
			xs <- sequence $ map (\_ -> prob) [1..10]
			print xs

distance :: (Double,Double) -> (Double,Double) -> Double
distance (x,y) (x',y') = ((x-x')**2 + (y-y')**2)**(0.5)
