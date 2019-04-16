module BinaryTree(
    BinaryTree(..),
    sizeBST,
    isBST,
    insert,
    successor,
    predecessor
    ) where 

import Utils.Triple as Triple

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

leftChild NIL = NIL
leftChild (Node _ left _) = left

rightChild NIL = NIL
rightChild (Node _ _ right) = right

getValue NIL = error "Cannot get value from a NIL Node"
getValue (Node key _ _) = key

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST bst = tripleFst (inIsBST bst)

inIsBST (Node key NIL NIL) = (Triple True key key)
inIsBST (Node key left NIL) 
    | lIsBST == True && key > lMaxKey = (Triple True lMinKey key)
    | otherwise = (Triple False lMinKey lMaxKey)
    where
        leftAns = inIsBST left
        lIsBST = tripleFst leftAns
        lMinKey = tripleSnd leftAns
        lMaxKey = tripleThr leftAns
inIsBST (Node key NIL right)
    | rIsBST == True && key < rMinKey = (Triple True key rMaxKey)
    | otherwise = (Triple False rMinKey rMaxKey)
    where
        rightAns = inIsBST right
        rIsBST = tripleFst rightAns
        rMinKey = tripleSnd rightAns
        rMaxKey = tripleThr rightAns
inIsBST (Node key left right)
    | rIsBST == True && lIsBST == True && key > lMaxKey && key < rMinKey = (Triple True lMinKey rMaxKey)
    | otherwise = (Triple False lMinKey rMaxKey) 
    where
        leftAns = inIsBST left
        lIsBST = tripleFst leftAns
        lMinKey = tripleSnd leftAns
        lMaxKey = tripleThr leftAns
        rightAns = inIsBST right
        rIsBST = tripleFst rightAns
        rMinKey = tripleSnd rightAns
        rMaxKey = tripleThr rightAns

--insere uma nova chave na BST retornando a BST modificada
insert NIL value = (Node value (NIL) (NIL))
insert (Node key left right) value = 
    if value < key 
        then (Node key (insert left value) right)
    else 
        if value > key 
            then (Node key left (insert right value))
            else (Node key left right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search NIL value = NIL
search (Node key left right) value = 
    if value == key
        then (Node key left right)
    else 
        if value < key 
            then search left value
        else search right value

--retorna o elemento maximo da BST
maxBST NIL = error "Cannot get maximum element from a NIL BinaryTree"
maxBST (Node key left NIL) = key
maxBST (Node key left right) = maxBST right

--retorna o elemento minimo da BST
minBST NIL = error "Cannot get minimum element from a NIL BinaryTree"
minBST (Node key NIL right) = key
minBST (Node key left right) = minBST left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor bst value
    | node == NIL = error "BST does not contains " ++ show value ++ " value"
    | otherwise = getValue (inPredecessor bst value)
    where
        node = search bst value

inPredecessor (Node key left right) value
    | key == value = if left == NIL then NIL else (Node (maxBST left) NIL NIL)
    | key > value = inPredecessor left value
    | otherwise = let rans = inPredecessor right value in if rans == NIL then (Node key NIL NIL) else rans

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor bst value
    | node == NIL = error "BST does not contains " ++ show value ++ " value"
    | otherwise = getValue (inSuccessor bst value)
    where
        node = search bst value

inSuccessor (Node key left right) value
    | key == value = if right == NIL then NIL else (Node (minBST right) NIL NIL)
    | key < value = inSuccessor right value
    | otherwise = let rans = inSuccessor left value in if rans == NIL then (Node key NIL NIL) else rans

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node key left right) = [key] ++ preOrder left ++ preOrder right

order NIL = []
order (Node key left right) = order left ++ [key] ++ order right

postOrder NIL = []
postOrder (Node key left right) = postOrder left ++ postOrder right ++ [key]