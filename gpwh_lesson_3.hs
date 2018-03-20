doubleDouble x = (\y -> 2*y) (2*x)

overwrite x = let x = 2
              in
                let x = 3
                in 
                  let x = 4
                  in
                    x

overwrite' x = (\_ -> 4) ((\_ -> 3) ((\_ -> 2) x))                    

sumSquareOrSquareSum x y = let sumSquare = (x^2 + y^2)
                               squareSum = (x + y)^2
                           in
                             if sumSquare > squareSum
                             then sumSquare
                             else squareSum

sumSquareOrSquareSum' x y = (\sumSquare squareSum ->
                             if sumSquare > squareSum
                             then sumSquare
                             else squareSum) (x^2 + y^2) ((x + y)^2)

x = 4
add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y -> (\x -> y + x) 1) 2

counter x = (\b -> b + 1) ((\a -> a + 1) x)

