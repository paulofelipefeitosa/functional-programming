xor :: Bool -> Bool -> Bool
xor x y = ((not x) && y) || (x && (not y))

xor' :: Bool -> Bool -> Bool
xor' True y = not y
xor' False y = y

xor'' :: Bool -> Bool -> Bool
xor'' x y | x == True = not y
    | x == False = y

impl :: Bool -> Bool -> Bool
impl x y = (not x) || y

equiv :: Bool -> Bool -> Bool
equiv x y = (impl x y) && (impl y x)

pow :: Int -> Int -> Int
pow x y
    | y < 0 = error "Cannot calculate the pow with a negative exponent value"
    | y == 0 = 1
    | y == 1 = x
    | (even y) == True = let midPow = pow x (quot y 2) in midPow * midPow
    | otherwise = let midPow = pow x (quot y 2) in midPow * midPow * x

factorial :: Int -> Int
factorial x 
    | x < 0 = error "Cannot calculate the factorial of a negative value"
    | x == 0 = 1
    | x == 1 = 1
    | otherwise = x * factorial (x-1)

sqrt'' :: Int -> Int -> Int -> Int -> Int
sqrt'' x l r lessGreater
    | l > r = lessGreater
    | p2mid == x = mid
    | p2mid < x = if mid > lessGreater then sqrt'' x (mid + 1) r mid else sqrt'' x (mid + 1) r lessGreater
    | p2mid > x = sqrt'' x l (mid - 1) lessGreater
    where 
        mid = (l + r) `div` 2
        p2mid = mid * mid

sqrt' :: Int -> Int
sqrt' x
    | x < 0 = error "Cannot calculate the sqrt of a negative value"
    | x == 0 = 0
    | x == 1 = 1
    | otherwise = sqrt'' x 1 (x `div` 2) 0

isPrime :: Int -> Bool
isPrime x 
    | x <= 0 = False
    | x == 1 = False
    | x == 2 = True
    | x == 3 = True
    | otherwise = all (0/=) (map (mod x) [2..factor])
    where 
        factor = sqrt' x

fib' :: Int -> [Int]
fib' 0 = [0]
fib' 1 = [1,0]
fib' x = let list = fib' (x-1) in (sum list) : (head list) : []

fib :: Int -> Int
fib x = head (fib' x)

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

mmc :: Int -> Int -> Int
mmc a b = (a * b) `div` (mdc a b)

coprime :: Int -> Int -> Bool
coprime a b = (mdc a b) == 1

goldbach :: Int -> [(Int, Int)]
goldbach x = [(xs, x - xs) | xs <- [2..(x `div` 2)], isPrime xs, isPrime (x - xs)]

main = do 
    print (xor True False)
    print (xor' True True)
    print (xor'' False False)
    print (xor'' False True)
    print (impl True False)
    print (impl True True)
    print (impl False True)
    print (impl False False)
    print (equiv True False)
    print (equiv True True)
    print (equiv False True)
    print (equiv False False)
    print (pow 3 2)
    print (factorial 5)
    print (factorial 0)
    print (isPrime 2)
    print (isPrime 1)
    print (isPrime 3)
    print (isPrime 10)
    print (isPrime 11)
    print (fib 50)
    print (mdc 12 8)
    print (mmc 12 8)
    print (coprime 35 64)
    print (goldbach 28)