-- Create Tree
data Tree a = Node a (Tree a) (Tree a) | Empty

-- Search tree
search :: Ord a => a -> Tree a -> Bool
search x Empty = False
search x (Node y l r)
 | x == y = True
 | x < y = search x l
 | x > y = search x r

--  insert into tree
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y l r)
    | x == y = Node y l r
    | x < y  = Node y (insert x l) r
    | x > y  = Node y l (insert x r)

-- Building Tree
buildtree :: (Ord a) => [a] -> Tree a
buildtree []     = Empty
buildtree (x:xs) = insert x (buildtree xs)

-- Delete Tree
delete' :: Ord a => Tree a -> Tree a
delete' (Node _ l Leaf)  =  l
delete' (Node _ Leaf r)  =  r
delete' (Node _ l r)     =  let (r', min) = delMin r in
                              Node min l r'
                              
-- Inorder traversal
treesort::  Ord a => [a] -> [a]
treesort xs = tree_inorder (list_to_bst xs)

tree_inorder:: Tree a -> [a]
tree_inorder Empty = []
tree_inorder (Node l v r) = tree_inorder l ++ [v] ++ tree_inorder r
-- Preorder traversal
treesort::  Ord a => [a] -> [a]
treesort xs = tree_preorder (list_to_bst xs)

tree_preorder:: Tree a -> [a]
tree_preorder Empty = []
tree_preorder (Node l v r) = [v] ++ tree_preorder l ++ tree_preorder r
-- Postorder traversal
treesort::  Ord a => [a] -> [a]
treesort xs = tree_postorder (list_to_bst xs)

tree_postorder:: Tree a -> [a]
tree_postorder Empty = []
tree_postorder (Node l v r) = tree_postorder l ++ tree_postorder r ++ [v]
-- if tree balanced
isBalanced :: BST a -> Bool
isBalanced tree = (maxDepth tree  - minDepth tree) <= 1

maxDepth :: BST a -> Int
maxDepth Empty = 0
maxDepth (Node l _ r) = 1 + max (maxDepth l) (maxDepth r)

minDepth :: BST a -> Int
minDepth Empty = 0
minDepth (Node l _ r) = 1 + min (minDepth l) (minDepth r)
-- average of the numbers in tree
bstAverage :: Tree x -> Int
bstAverage Empty = 0
bstAverage tree = totalSum(tree) / numberOfNodes(tree)

-- Number of Nodes
numberOfNodes :: Tree x -> Int
numberOfNodes Empty = 0
numberOfNodes (Node _ l r) = 1 + numberOfNodes(st1) + numberOfNodes(st2)

-- total sum
totalSum :: Tree a -> Int
totalSum Empty = 0
totalSum (Node x l r) = n + totalSum(l) + totalSum(r) 

-- height
height :: Tree a -> Int
height Empty        = 0
height (Node x l r) = 1 + (max (height l) (height r))

-- print contents
elements :: Tree a -> [a]
elements Empty = []
elements (Node x l r) = (elements l) ++ (x : elements r)

-- List to BST
list_to_bst:: Ord a => [a] -> Tree a
list_to_bst [] = Empty
list_to_bst (x:xs) = bst_insert x (list_to_bst xs)