import System.Random

point :: IO (Double,Double)
point = do 
			d <- randomRIO (0,1/2)
			theta <- randomRIO(0,pi/2)
			return (d,theta)



f d t = (d < (sin t)/2)
experiment = do 
				let n = 1000000
				xs <- sequence [point | _ <- [1..n]]
				let xs' = filter (uncurry f) xs
				let t = fromIntegral (length xs') / (fromIntegral (length xs))
				print $ (2/t)


let remaining = case (parse startParser' "" rest) of
                                                (Left _) ->  []
                                                (Right ns) -> ns