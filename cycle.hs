cyclic n = let x = [1..n] : y;
                y =  x
             in x   

cyc n = concat (cyclic n)


