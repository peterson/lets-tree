{-

Lets.BinomialHeap

A binomial heap is a heap consisting of a _collection_ of binomial trees. In
particular, a binomial heap supports an efficient merge operations, allowing
multiple heaps to be rapidly merged to form a new heap.

Invariants

1. Minimum heap property: the value of each node is less than, or equal to, the
   value of each of its children.

2. Rank property: there can be at most 1 binary tree of each rank, including
   zero rank.

Based on the "Binomial Heaps" January Haskell Test (2013) by Tony Field,
Reader in Performance Engineering, Imperial College London.

References

[1] Wikipedia, "Binomial Heaps", http://en.wikipedia.org/wiki/Binomial_heap

[2] Tony Field, "Binomial Heaps" (2013), http://wp.doc.ic.ac.uk/ajf/binomial-heaps
    A PDF of the spec available at: http://www.doc.ic.ac.uk/~ajf/haskelltests/binheaps/spec.pdf

-}

module Lets.BinomialHeap
  ( BinHeap

  -- constructors
  , empty
  , singleton

  -- properties

  -- operations
  , insert
  , fromList
  , toList
  , binSort

  )
  where

import Data.List ((\\)) -- (\\) is the list difference operator


--
-- definition
--

data BinTree a = Node Int a [BinTree a] -- n.b. changed order of Int and a, relative to [2]
               deriving (Eq, Show, Ord)

type BinHeap a = [BinTree a] -- n.b. 'type' definition, not 'data' definition !


--
-- operations
--

value :: BinTree a -> a
value (Node _ v _) = v

rank :: BinTree a -> Int
rank (Node r _ _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

node :: a -> BinTree a
node v = Node 0 v []

singleton :: a -> BinHeap a
singleton v = [node v]

empty :: BinHeap a
empty = []

--
-- combine takes two trees of rank n, and produces a single tree of rank n+1

combine :: Ord a => BinTree a -> BinTree a -> BinTree a
combine t1@(Node r v1 c1) t2@(Node _ v2 c2)
  | v1 < v2   = Node (r+1) v1 (t2 : c1)
  | otherwise = Node (r+1) v2 (t1 : c2)


--
-- merge takes two binary heaps and produces a single (merged) heap

merge :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(t1:ts1) h2@(t2:ts2)
  | rank t1 < rank t2 = t1 : merge ts1 h2
  | rank t2 < rank t1 = t2 : merge h1 ts2
  | otherwise         = merge [combine t1 t2] $ merge ts1 ts2


--
-- insert a value into the heap

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v h = merge (singleton v) h


--
-- extract the minimum value in heap by checking for the minimum of
-- the values of the root of each tree in the heap.

extractMin :: Ord a => BinHeap a -> a
extractMin h = minimum $ value <$> h


--
-- delete minimum value from the heap

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h = merge ch h'
  where
    mv = extractMin h          -- min val
    mt = treeWithRoot mv h     -- min tree
    h' = h \\ [mt]             -- heap remainder
    ch = reverse $ children mt -- child heap


--
-- treeWithRoot takes a value and a heap and returns the first tree in the heap
-- with the value v.

treeWithRoot :: Eq a => a -> BinHeap a -> BinTree a
treeWithRoot v h = head $ filter (\x -> value x == v) h


--
-- binomial sorting
--
-- > binSort "BinomialHeap"
-- > "BHaaeiilmnop"

fromList :: Ord a => [a] -> BinHeap a
fromList = foldr insert empty

toList :: Ord a => BinHeap a -> [a]
toList [] = []
toList h = extractMin h : toList (deleteMin h)

binSort :: Ord a => [a] -> [a]
binSort = toList . fromList


--
-- sample trees and heaps for testing purposes, drawn from reference [2]
--
-- note: the order of rank and value fields has been changed (from the original
-- in [2]) to reflect our local data type definition.


--
-- sample trees
--

t1 = Node 0 4 []
t2 = Node 1 1 [Node 0 5 []]
t3 = Node 2 2 [Node 1 8 [Node 0 9 []],
               Node 0 7 []]
t4 = Node 3 2 [Node 2 3 [Node 1 6 [Node 0 8 []],
                         Node 0 10 []],
               Node 1 8 [Node 0 9 []],
               Node 0 7 []]

-- t5 and t6 are on the left of Figure 2; t7 is on the
-- right
t5 = Node 2 4 [Node 1 6 [Node 0 8 []],
                         Node 0 10 []]
t6 = Node 2 2 [Node 1 8 [Node 0 9 []], Node 0 7 []]
t7 = Node 3 2 [Node 2 4 [Node 1 6 [Node 0 8 []], Node 0 10 []],
               Node 1 8 [Node 0 9 []],
               Node 0 7 []]

-- An additional tree...
t8 = Node 1 12 [Node 0 16 []]


--
-- sample heaps
--

-- Two arbitrary heaps for testing...
h1 = [t2, t7]
h2 = [Node 2 1 [Node 1 12 [Node 0 16 []],
                Node 0 5 []],
      Node 3 2 [Node 2 4 [Node 1 6 [Node 0 8 []],
                          Node 0 10 []],
                Node 1 8 [Node 0 9 []],
                Node 0 7 []]]

-- h3 is shown in Figure 3...
h3 = [t1, t2, t4]

-- Two additional heaps, used below. They are shown
-- in Figure 4(a)...

h4 = [t2, t5]
h5 = [t1, t8]

-- h6 is the result of merging h4 and h5, shown in Figure 4(b)...
h6 = [Node 0 4 [],
      Node 3 1 [Node 2 4 [Node 1 6 [Node 0 8 []],
                          Node 0 10 []],
                Node 1 12 [Node 0 16 []],
                Node 0 5 []]]

-- h7 is shown in Figure 5...
h7 = [Node 3 4 [Node 2 4 [Node 1 12 [Node 0 16 []],
                          Node 0 5 []],
                Node 1 6 [Node 0 8 []],
                Node 0 10 []]]
