
divTuple (x, y) = x / y

somatorio :: Int -> Int -> Int
somatorio l r = sum [l..r]

somatorioRec :: Int -> Int -> Int
somatorioRec l r 
    | l == r = l
    | l > r = r + somatorioRec l (r + 1)
    | otherwise = l + somatorioRec (l + 1) r

square x = x * x

sumSquares x y = square x + square y

higherOrderSum f a b = sum (map f [a, b])

hoSumSquares x y = higherOrderSum square x y

mapFilter f p xs = filter p (map f xs)

main = do 
    print (divTuple (10, 3))
    print (divTuple (5.0, 3.0))
    print (somatorio 1 3)
    print (somatorio 1 3 == somatorioRec 3 1)
    print (somatorioRec 1 1)
    print (somatorioRec 0 0)
    print (somatorioRec 1 3)
    print (somatorioRec 3 1)
    print (square 4)
    print (sumSquares 4 3)
    print (higherOrderSum square 4 3)
    print (hoSumSquares 4 3)
    print (mapFilter square (10<) [1, 2, 3, 4, 5])