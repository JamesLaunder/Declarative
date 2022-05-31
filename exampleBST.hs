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

-- Inorder traversal
treesort::  Ord a => [a] -> [a]
treesort xs = tree_inorder (list_to_bst xs)

tree_inorder:: Tree a -> [a]
tree_inorder Empty = []
tree_inorder (Node l v r) = tree_inorder l ++ [v] ++ tree_inorder r
-- Preorder traversal

-- Postorder traversal

-- if tree balanced

-- average of the numbers in tree

-- total count

-- height
height :: Tree a -> Int
height Empty        = 0
height (Node x l r) = 1 + (max (height l) (height r))

-- print contents
elements :: Tree a -> [a]
elements Empty = []
elements (Node x l r) = (elements l) ++ (x : elements r)