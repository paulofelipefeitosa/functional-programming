module MultisetList ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import qualified Data.List as List
import Control.Exception
import Prelude hiding (sum) 
import qualified Multiset as Multiset

data MultisetList a = MultisetList [(a,Int)] deriving (Eq,Show)

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
instance Multiset.Multiset MultisetList where
    insert elem (MultisetList []) = MultisetList [(elem, 1)]
    insert elem (MultisetList list) = MultisetList (List.insert (elem, xq+1) (List.delete (elem, xq) list))
        where
            xq = Multiset.search elem (MultisetList list)

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
    remove elem (MultisetList list)
        | xq == 0 = throwIO (Multiset.ElementoInexistente ("Cannot remove " ++ show elem))
        | xq == 1 = return (MultisetList (List.delete (elem, xq) list))
        | otherwise = return (MultisetList (List.insert (elem, xq-1) (List.delete (elem, xq) list)))
        where
            xq = Multiset.search elem (MultisetList list)

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
    search elem (MultisetList []) = 0
    search elem (MultisetList ((x,xq):xs))
        | x == elem = xq
        | otherwise = Multiset.search elem (MultisetList xs)

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
 -}
    union bag (MultisetList []) = bag
    union (MultisetList []) bag = bag
    union (MultisetList (hx:xs)) (MultisetList (hy:ys)) 
        | x == y = if xq >= yq 
            then mlConcat (MultisetList [(x,xq)]) (Multiset.union (MultisetList xs) (MultisetList ys))
            else mlConcat (MultisetList [(y,yq)]) (Multiset.union (MultisetList xs) (MultisetList ys))
        | x < y = mlConcat (MultisetList [(x,xq)]) (Multiset.union (MultisetList xs) (MultisetList (hy:ys)))
        | otherwise = mlConcat (MultisetList [(y,yq)]) (Multiset.union (MultisetList (hx:xs)) (MultisetList ys))
        where
            x = fst hx
            xq = snd hx
            y = fst hy
            yq = snd hy

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
 -}
    intersection (MultisetList []) bag = MultisetList []
    intersection bag (MultisetList []) = MultisetList []
    intersection (MultisetList (hx:xs)) (MultisetList (hy:ys))
        | x == y = if xq <= yq 
            then mlConcat (MultisetList [(x,xq)]) (Multiset.intersection (MultisetList xs) (MultisetList ys))
            else mlConcat (MultisetList [(y,yq)]) (Multiset.intersection (MultisetList xs) (MultisetList ys))
        | x < y = Multiset.intersection (MultisetList xs) (MultisetList (hy:ys))
        | otherwise = Multiset.intersection (MultisetList (hx:xs)) (MultisetList ys)
        where
            x = fst hx
            xq = snd hx
            y = fst hy
            yq = snd hy

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
    minus bag (MultisetList []) = return bag
    minus (MultisetList []) bag = return (MultisetList [])
    minus (MultisetList ((x,xq):xs)) (MultisetList ((y,yq):ys))
        | x == y && xq < yq = throwIO (Multiset.OperacaoNaoPermitida (show x ++ " in the first bag has less occurrences [" ++ show xq ++ "] than second bag [" ++ show yq ++ "]"))
        | x == y && xq == yq = Multiset.minus (MultisetList xs) (MultisetList ys)
        | x == y && xq > yq = return (mlConcat (MultisetList [(x,xq - yq)]) (Multiset.minus (MultisetList xs) (MultisetList ys)))
        | x < y = return (mlConcat (MultisetList [(x,xq)]) (Multiset.minus (MultisetList xs) (MultisetList ((y,yq):ys))))
        | otherwise = Multiset.minus (MultisetList ((x,xq):xs)) (MultisetList ys)

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
    inclusion (MultisetList []) bag = True
    inclusion bag (MultisetList []) = False
    inclusion (MultisetList ((x,xq):xs)) (MultisetList ((y,yq):ys))
        | x == y = if xq <= yq then Multiset.inclusion (MultisetList xs) (MultisetList ys) else False
        | x < y = False
        | otherwise = Multiset.inclusion (MultisetList ((x,xq):xs)) (MultisetList ys)


{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
 -}
    sum bag (MultisetList []) = bag
    sum (MultisetList []) bag = bag
    sum (MultisetList ((x,xq):xs)) (MultisetList ((y,yq):ys))
        | x == y = mlConcat (MultisetList [(x,xq + yq)]) (Multiset.sum (MultisetList xs) (MultisetList ys))
        | x < y = mlConcat (MultisetList [(x,xq)]) (Multiset.sum (MultisetList xs) (MultisetList ((y,yq):ys)))
        | otherwise = mlConcat (MultisetList [(y,yq)]) (Multiset.sum (MultisetList ((x,xq):xs)) (MultisetList ys))

{-
 - Retorna a quantidade total de elementos no Bag
 -}
    size (MultisetList []) = 0
    size (MultisetList ((x,xq):xs)) = xq + (Multiset.size (MultisetList xs))

mlConcat :: MultisetList bag -> IO (MultisetList bag) -> IO (MultisetList bag)
mlConcat (MultisetList bag) (MultisetList otherBag) = return (MultisetList (bag ++ otherBag))