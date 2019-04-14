
meuLast :: [t] -> t
meuLast [] = error "Lista vazia!"
meuLast [x] = x
meuLast (x:xs) = meuLast xs

penultimo :: [t] -> t
penultimo [] = error "Lista sem penultimo"
penultimo [x] = error "Lista sem penultimo"
penultimo xs = meuLast (init xs)

elementAt i xs = meuLast (take i xs)

meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs

meuReverso :: [t] -> [t]
meuReverso [] = []
meuReverso (x:xs) = meuReverso xs ++ [x]

isPalindrome :: (Eq t) => [t] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs = let 
        size = meuLength xs
        midSize = size `div` 2 
    in if (mod size 2) == 0 
        then (take midSize xs) == (reverse (drop midSize xs)) 
        else (take midSize xs) == (reverse (drop (midSize + 1) xs))

compress' init [] = init
compress' init (y:ys) = if elem y init 
    then compress' init ys 
    else compress' (init ++ [y]) ys

compress xs = compress' [] xs

equals a b = if a == b then 1 else 0

count x xs = sum (map (equals x) xs)

compact [] = []
compact (x:xs) = replicate ((count x xs) + 1) x ++ compact ([y | y <- xs, y/=x])

encode [] = []
encode (x:xs) = [(x, 1 + count x xs)] ++ encode ([y | y <- xs, y/=x])

split _ 0 = error "Cannot split list in zero index"
split [] _ = [[], []]
split (x:xs) 1 = [[x], xs]
split (x:xs) i = let result = split xs (i-1) in [[x] ++ head result, meuLast result]

slice (x:xs) 1 1 = [x]
slice (x:xs) 1 r = [x] ++ slice xs 1 (r - 1) 
slice (x:xs) l r = slice xs (l - 1) (r - 1)

insertAt el 1 [] = [el] 
insertAt el 1 (x:xs) = el : x : xs 
insertAt el pos (x:xs) = x : insertAt el (pos -  1) xs

minList [x] = x
minList (x:xs) = let min = minList xs in if x < min then x else min

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)

sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 

mySum (x:[]) = x
mySum (x:xs) = x + (mySum xs)

maxList [] = error "Cannot calculate the maximum element of an empty list"
maxList (x:[]) = x
maxList (x:xs) = foldr (max) x xs

buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]  

mean (x:[]) = x
mean (x:xs) = (x + ((mean xs) * (myLgth - 1))) / myLgth
    where
        myLgth = (meuLength xs) + 1

myAppend xs (y:ys) = (foldr (:) y xs) {-: [] foldr (:) (last ys) (init ys)-}

main = do
    print (meuLast [1])
    print (meuLast [1, 2])
    print (penultimo [1, 2, 3, 4])
    print (penultimo [1, 2])
    print (elementAt 2 [1, 2, 3, 4])
    print (elementAt 2 [4, 7, 1, 9])
    print (meuLength [4, 7, 1, 9])
    print (meuReverso [4, 7, 1, 9])
    print (isPalindrome [1])
    print (isPalindrome [1, 1])
    print (isPalindrome [1, 2])
    print (isPalindrome [1, 2, 1])
    print (isPalindrome [1, 2, 2])
    print (isPalindrome [1, 2, 2, 1])
    print (isPalindrome [1, 2, 1, 2])
    print (compress [1, 2, 1, 2])
    print (compress [1])
    print (compress [1, 3])
    print (compact [2,5,8,2,1,8])
    print (compact [2,1])
    print (encode [2,2,2,3,4,2,5,2,4,5])
    print (encode [1])
    print (split [1, 2, 3, 4] 1)
    print (split [1, 2, 3, 4] 4)
    print (split [1, 2, 3, 4] 5)
    print (split [1, 2, 3, 4] 3)
    print (slice [1, 2, 3, 4] 1 2)
    print (slice [1, 2, 3, 4] 2 3)
    print (slice [1, 2, 3, 4] 3 4)
    print (slice [1, 2, 3, 4] 4 4)
    print (slice [1, 2, 3, 4] 1 1)
    print (insertAt 10 1 [1, 2, 3, 4])
    print (insertAt 10 4 [1, 2, 3, 4])
    print (insertAt 10 2 [1, 2, 3, 4])
    print (insertAt 10 5 [1, 2, 3, 4])
    print (sort [2, -3, 4, 1, 20, 3, 10])
    print (mySum [2, -3, 4, 1, 20, 3, 10])
    print (mySum [1])
    print (maxList [2, -3, 4, 1, 20, 3, 10])
    print (maxList [-3])
    print (buildPalindrome [1,2,3])
    print (mean [2, -3, 4, 1, 20, 3, 10])
    print (myAppend [1, 2] [3])
    print (myAppend [1, 2] [3, 4])