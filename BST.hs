data BST a = Empty | Node (BST a) a (BST a)

-- Write your isBalanced function here
height :: BST a -> Integer
height Empty = 1
height (Node (left z) x (right y)) = 1 + max (height (left x)) (height (right y))







-- data Tree = Node Tree Int Tree | Leaf Int deriving Show

-- height :: Tree -> Integer
-- height (Leaf _) = 1
-- height (Node left x right) = 1 + max (height left) (height right)

-- isBalancedTree :: Tree -> Bool
-- isBalanced (Leaf _) = True 
-- isBalancedTree (Node left x right) = 
--     let diff = abs (height left - height right) in
--     diff <= 1 && isBalancedTree left && isBalancedTree right

