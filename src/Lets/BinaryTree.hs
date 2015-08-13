{-

Lets.BinaryTree

A basic self-balancing binary tree.

References

[1] Wikipedia, "Binary Tree", http://en.wikipedia.org/wiki/Binary_tree

[2] John Hargrove, "The AVL Tree Rotations Tutorial", version 1.0.1, March 2007.
PDF available at: http://pages.cs.wisc.edu/~paton/readings/liblitVersion/AVL-Tree-Rotations.pdf

-}

module Lets.BinaryTree
  ( Tree

  -- constructors
  , leaf
  , node
  , empty

  -- properties
  , value
  , size
  , height
  , ordered
  , balanced

  -- operations
  , insert
  , fromList
  , sample
  )
  where

import Data.Maybe (fromJust)

--
-- definitions
--

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

--
-- constructors
--

leaf :: Tree a
leaf = Leaf

node :: a -> Tree a
node v = Node Leaf v Leaf

empty :: Tree a
empty = leaf

-- children :: Tree a -> Maybe (Tree a,Tree a)
-- children Leaf = Nothing
-- children (Node l v r) = Just (l,r)

--
-- properties
--

value :: Tree a -> Maybe a
value Leaf = Nothing
value (Node _ v _) = Just v


height :: Tree a -> Int
height Leaf = 0
height (Node Leaf _ Leaf) = 1
height (Node l v r) = 1 + max (height l) (height r)


size :: Tree a -> Int
size Leaf = 0
size (Node l v r) = 1 + size l + size r


ordered :: Ord a => Tree a -> Bool
ordered Leaf = True
ordered (Node Leaf _ Leaf) = True
ordered (Node Leaf v r) = v <= (fromJust $ value r) && ordered r
ordered (Node l v Leaf) = (fromJust $ value l) <= v && ordered l
ordered (Node l v r) = (fromJust $ value l) <= v && v <= (fromJust $ value r) && ordered l && ordered r


balanced :: Tree a -> Bool
balanced (Leaf) = True
balanced (Node l _ r)=
  bal && balanced l && balanced r
  where
    bal = abs (height l - height r) <= 1


--
-- operations
--

insert :: Ord a => a -> Tree a -> Tree a
insert a t = rebalance t'
  where t' = insert' a t -- insert' returns a potentially unbalanced tree


insert' :: Ord a => a -> Tree a -> Tree a
insert' a Leaf = node a
insert' a (Node l v r)
  | a <= v    = Node (insert a l) v r
  | otherwise = Node l v (insert a r)


rebalance :: Tree a -> Tree a
rebalance Leaf = Leaf
rebalance t@(Node l v r)
  | leftHeavy t && rightHeavy l = rotateRL t -- double rotation
  | rightHeavy t && leftHeavy r = rotateLR t -- double rotation
  | leftHeavy t                 = rotateR t  -- single rotation
  | rightHeavy t                = rotateL t  -- single rotation
  | otherwise                   = t          -- do nothing
  where
    leftHeavy  (Node l _ r) = heavy l r
    rightHeavy (Node l _ r) = heavy r l
    heavy a b = height a > height b


--
-- build a tree from a list

fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty

--
-- build a sample tree

sample :: Tree Integer
sample = foldr insert empty (reverse [1..20])


--
-- Tree Rotations
--
-- In order for a binary tree to remain balanced as additional elements are
-- inserted, a combination of single (and more occasionally, double) tree
-- rotations are performed to transform the shape of the tree's sub-trees.
-- Reference [2] provides a good guide to these rotation operations, and the
-- rotations are implemented below, using pattern-match syntax.
--

--
-- perform a left rotation on a tree
--
--      B           A
--     / \         / \
--    D  A   =>   B  C
--      / \      / \
--     E  C     D  E
--

rotateL :: Tree a -> Tree a
rotateL (Node d b Leaf) = Node d b Leaf
rotateL (Node d b (Node e a c)) = Node (Node d b e) a c


--
-- perform a right rotation on a tree
--
--      A           B
--     / \         / \
--    B  C   =>   D  A
--   / \            / \
--  D  E           E  C
--

rotateR :: Tree a -> Tree a
rotateR (Node Leaf a c) = Node Leaf a c
rotateR (Node (Node d b e) a c) = Node d b (Node e a c)


--
-- perform a RL "double rotation"
--
-- RL => rotate left sub-tree to the left, followed by the root of tree to the right

rotateRL :: Tree a -> Tree a
rotateRL (Node l v r) = rotateR (Node (rotateL l) v r)


--
-- perform a LR "double rotation"
--
-- LR => rotate right sub-tree to the right, followed by the root of tree to the left

rotateLR :: Tree a -> Tree a
rotateLR (Node l v r) = rotateL (Node l v (rotateR r))
