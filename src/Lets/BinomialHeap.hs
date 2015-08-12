{-

Lets.BinomialHeap

A binomial heap is a heap consisting of a _collection_ of binomial trees. In
particular, a binomial heap provides an efficient merge operation, allowing
multiple heaps to be merged rapidly to form a new heap.

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
  , node

  -- properties
  , value
  , rank
  , children
  , size

  -- operations
  , insert
  , deleteMin
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
-- constructors
--

empty :: BinHeap a
empty = []

singleton :: a -> BinHeap a
singleton v = [node v]

node :: a -> BinTree a
node v = Node 0 v []


--
-- properties
--

value :: BinTree a -> a
value (Node _ v _) = v

rank :: BinTree a -> Int
rank (Node r _ _) = r

children :: BinTree a -> [BinTree a]
children (Node _ _ c) = c

size :: Ord a => BinHeap a -> Int
size = length . toList


--
-- operations
--

--
-- | Takes two trees of rank /n/ and combines them into a single tree of rank /n+1/.

combine :: Ord a => BinTree a -> BinTree a -> BinTree a
combine t1@(Node r v1 c1) t2@(Node _ v2 c2)
  | v1 < v2   = Node (r+1) v1 (t2 : c1)
  | otherwise = Node (r+1) v2 (t1 : c2)


--
-- | Takes two binomial heaps and merges them into a single binomial heap.

merge :: Ord a => BinHeap a -> BinHeap a -> BinHeap a
merge h1 [] = h1
merge [] h2 = h2
merge h1@(t1:ts1) h2@(t2:ts2)
  | rank t1 < rank t2 = t1 : merge ts1 h2
  | rank t2 < rank t1 = t2 : merge h1 ts2
  | otherwise         = merge [combine t1 t2] $ merge ts1 ts2


--
-- | Inserts a value into the heap. This is done by creating a singleton heap
-- for the value and then merging this with the existing heap.

insert :: Ord a => a -> BinHeap a -> BinHeap a
insert v h = merge (singleton v) h


--
-- | Extracts the minimum value in the heap. This is done by checking for the
-- value amongst the root nodes of each tree in the heap. This makes use of the
-- property that the minimum value is always stored in one of the root nodes of
-- the heap's trees.

extractMin :: Ord a => BinHeap a -> a
extractMin h = minimum $ value <$> h


--
-- | Deletes the minimum value from the heap. This is done by merging the remainder
-- of the heap with a new heap that is formed from from children (if any) of the
-- node to be removed.

deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin h = merge ch h'
  where
    mv = extractMin h          -- min val
    mt = treeWithRoot mv h     -- min tree
    h' = h \\ [mt]             -- heap remainder
    ch = reverse $ children mt -- child heap


--
-- | Takes a value and a heap and returns the /first/ tree in the heap that has
-- a root node of this value.

treeWithRoot :: Eq a => a -> BinHeap a -> BinTree a
treeWithRoot v h = head $ filter (\x -> value x == v) h


-- | Builds a binomial heap from a list.

fromList :: Ord a => [a] -> BinHeap a
fromList = foldr insert empty


-- | Builds an ordered list from a binomial heap.

toList :: Ord a => BinHeap a -> [a]
toList [] = []
toList h = extractMin h : toList (deleteMin h)


--
-- | Performs a sort operation using the binomial heap. This is done by firstly
-- building a heap from a (probably unordered) list, then recursively extracting
-- the elements in order to produce a sorted list.
--
-- >>> binSort "BinomialHeap"
-- "BHaaeiilmnop"

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
