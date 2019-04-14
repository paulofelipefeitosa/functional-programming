module BinaryTree(
    BinaryTree(..),
    sizeBST,
    isBST,
    insert
    ) where 

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

leftChild NIL = NIL
leftChild (Node _ left _) = left
rightChild NIL = NIL
rightChild (Node _ _ right) = right

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST = undefined

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
    | node == NIL = error "BST does not contains " ++ show value
    | otherwise = minBST (rightChild node)
    where
        node = search bst value

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined