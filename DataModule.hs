data Triple a b c = Nada | Triple a b c deriving (Eq,Show)

tripleFst (Nada) = error "Cannot get 'tripleFst' from Nothing"
tripleFst (Triple a b c) = a
tripleSnd (Nada) = error "Cannot get 'tripleFst' from Nothing"
tripleSnd (Triple a b c) = b
tripleThr (Nada) = error "Cannot get 'tripleFst' from Nothing"
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b | Vazio deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a, b)
secondTwo (Quadruple a b c d) = (c, d)

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d | NVazio deriving (Eq,Show)

tuple1 (Tuple1 a) = a
tuple1 (Tuple2 a _) = a
tuple1 (Tuple3 a _ _) = a
tuple1 (Tuple4 a _ _ _) = a

tuple2 (Tuple2 _ b) = b
tuple2 (Tuple3 _ b _) = b
tuple2 (Tuple4 _ b _ _) = b
tuple2 _ = Nothing

tuple3 (Tuple3 _ _ c) = c
tuple3 (Tuple4 _ _ c _) = c
tuple3 _ = Nothing

tuple4 (Tuple4 _ _ _ d) = d
tuple4 _ = Nothing

main = do
    print (tripleFst (Triple 10 20 30))
    print (tripleSnd (Triple 10 20 30))
    print (tripleThr (Triple 10 20 30))
    print (firstTwo (Quadruple 10 20 "paulo" "felipe"))
    print (secondTwo (Quadruple 10 20 "paulo" "felipe"))
    