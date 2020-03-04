primes = sieve [2..]
sieve (x:xs) = x : sieve ([y | y <- xs, y `mod` x /= 0])


f (x,y) = x : f (y,x+y) 
fib = f (1,1)

g (x,y,1) = x
g (x,y,n) = g (y,x+y,n-1)

fib' n = g(1,1,n)



r :: [Int] -> [Int]
r lst = do 
			x <- lst
			case (mod x 3) of {
    				0 -> [x];
    				1 -> [x,x];
                    2 -> [x,x,x] }
