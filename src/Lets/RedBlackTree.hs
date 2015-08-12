{-

Lets.RedBlackTree

A simple Red-Black Tree.


Invariants:

  1. No red node has a red parent
  2. Every path from the root to a leaf contains the _same_
     number of black nodes.


References:

  [1] Wikipedia, "Red-Black Tree", http://en.wikipedia.org/wiki/Red%E2%80%93black_tree

  [2] Chris Okasaki, "Red-Black Trees in a Functional Setting",
      Journal of Functional Programming, 9(4):471-477, July 1999.
      PDF available at http://bit.ly/1MXNTfW

-}

module Lets.RedBlackTree
  ( RBTree

  -- constructors
  , empty
  , node
  , singleton

  -- properties

  -- operations
  , insert
  , fromList
  , sample
  )
  where

--
-- definitions
--

data C = R -- Red
       | B -- Black
       deriving (Eq, Show)

data RBTree a = Leaf
              | Node C (RBTree a) a (RBTree a)
              deriving (Eq, Show)


--
-- constructors
--

-- | /O(1)/. Create an empty tree.
empty :: RBTree a
empty = Leaf

-- | /O(1)/. Create a node, red by default.
node :: a -> RBTree a
node v = Node R Leaf v Leaf

-- | /O(1)/. Create a singleton tree.
singleton :: a -> RBTree a
singleton v = Node B Leaf v Leaf

--
-- operations
--

black :: RBTree a -> RBTree a
black (Node _ l v r) = (Node B l v r) -- repaint black

red :: RBTree a -> RBTree a
red   (Node _ l v r) = (Node R l v r) -- repaint red

insert :: Ord a => a -> RBTree a -> RBTree a
insert x t = black (ins x t)
  where
    ins x Leaf = node x
    ins x t@(Node c l v r)
      | x <  v = rebalance (Node c (ins x l) v r)
      | x == v = rebalance t
      | x >  v = rebalance (Node c l v (ins x r))

member :: Ord a => a -> RBTree a -> Bool
member x Leaf = False
member x (Node _ l v r)
  | x <  v = member x l
  | x >  v = member x r
  | x == v = True


--
-- rebalance
--
-- See [2] for a detailed exposition of this algorithm.

rebalance (Node B (Node R (Node R a x b) y c) z d) =
  Node R (Node B a x b) y (Node B c z d)
rebalance (Node B (Node R a x (Node R b y c)) z d) =
  Node R (Node B a x b) y (Node B c z d)
rebalance (Node B a x (Node R (Node R b y c) z d)) =
  Node R (Node B a x b) y (Node B c z d)
rebalance (Node B a x (Node R b y (Node R c z d))) =
  Node R (Node B a x b) y (Node B c z d)
rebalance t@(Node _ _ _ _) = t -- otherwise no change


-- build a tree
fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert empty


-- sample tree
sample :: RBTree Integer
sample = foldr insert empty (reverse [1..20])
