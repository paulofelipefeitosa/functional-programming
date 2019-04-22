module MultisetMap ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.Map, onde 
 - cada elemento da lista consiste do dado em si mapeado para sua quantidade. 
 - Eh recomendavel que voce consulte a documentacao de Data.Map
 -}
import qualified Data.Map as Map

data MultisetMap a = MultisetMap (Map.Map a Int) deriving (Eq,Show)

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem (MultisetMap map) = (MultisetMap (Map.insertWith (+) elem 1 map))

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem (MultisetMap map) = let f k v = if v == 1 then Nothing else Just (v-1) 
    in (MultisetMap (Map.updateWithKey f elem map))

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem (MultisetMap map) = Map.findWithDefault 0 elem map

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union (MultisetMap map) (MultisetMap otherMap) = (MultisetMap (Map.unionWith (max) map otherMap))

{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection (MultisetMap map) (MultisetMap otherMap) = (MultisetMap (Map.intersectionWith (min) map otherMap))

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve ser removido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus (MultisetMap map) (MultisetMap otherMap) = let f vl vr = if vl > vr then Just (vl - vr) else Nothing 
    in (MultisetMap (Map.differenceWith f map otherMap))

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion (MultisetMap map) (MultisetMap otherMap) = otherMap == (Map.unionWith (max) map otherMap)

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sum (MultisetMap map) (MultisetMap otherMap) = (MultisetMap (Map.unionWith (+) map otherMap))

{-
 - Retorna a quantidade total de elementos no Bag
-}
size (MultisetMap map) = let f v acc = v + acc in (Map.foldr f 0 map)