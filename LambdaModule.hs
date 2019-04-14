pow = \x y -> 
    if (y == 0) then 1 
    else (\midPow isEven -> midPow * midPow * (if isEven then 1 else x)) (pow x (quot y 2)) (even y)

fact = \x -> if (x == 0) then 1 else x*(fact (x-1))

isPrime' = \n factor -> (if (factor == 1) 
    then True 
    else (length [y | y <- [2..factor], (mod n y) == 0]) == 0)

isPrime = \x -> (if x <= 1 
    then False 
    else isPrime' x (floor (sqrt (fromIntegral x))))

fib' = \x -> (if (x == 1) then [1, 0] else (\list -> [sum list, head list]) (fib' (x-1)))

fib = \x -> (if (x == 0) then 0 else head (fib' x))

meuLast = \xs -> (case xs of
    x:[] -> x
    x:xs -> meuLast xs)

penultimo = \xs -> (meuLast (init xs))

elementAt = \i xs -> (if i == 1 then head xs else elementAt (i-1) (tail xs))

main = do
    print (pow 2 0)
    print (pow 2 1)
    print (pow 2 2)
    print (pow 2 3)
    print (pow 3 4)
    print (fact 0)
    print (fact 1)
    print (fact 2)
    print (fact 3)
    print (fact 4)
    print (isPrime 2)
    print (isPrime 3)
    print (isPrime 4)
    print (isPrime 1)
    print (isPrime 5)
    print (fib 0)
    print (fib 1)
    print (fib 2)
    print (fib 3)
    print (fib 4)
    print (fib 5)
    print (fib 6)
    print (fib 7)
    print (meuLast [1])
    print (meuLast [1, 2])
    print (meuLast [1, 2, 3])
    print (penultimo [1, 2, 3])
    print (elementAt 1 [1, 2, 3])
    print (elementAt 3 [1, 2, 3])
    print (elementAt 2 [1, 2, 3])