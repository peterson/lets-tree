{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BinomialHeap where

import           Control.Monad     (liftM)
import           Data.List         (nub, sort, (\\))
import           Lets.BinomialHeap
import           Test.QuickCheck


instance (Ord a, Arbitrary a) => Arbitrary (BinHeap a) where
  arbitrary = liftM fromList arbitrary


-- binomial heaps contain at most 1 tree of rank r
prop_heap_one_tree_per_rank :: BinHeap Int -> Bool
prop_heap_one_tree_per_rank ts =
  nub ranks == ranks -- no dups
  where
    ranks = rank <$> ts


-- if total number of nodes is N, then the heap contains at most ⌊log2(N)⌋+1 trees
prop_heap_max_trees :: BinHeap Int -> Bool
prop_heap_max_trees ts =
  count <= limit
  where
    count = length ts
    limit = floor $ lg (fromIntegral $ size ts) + 1
    lg = logBase (2 :: Double) -- added :: to squish warning


-- a heap with trees of rank 1, 2, .. k contains i=1..k, sum(2^r_i) nodes
prop_heap_size :: BinHeap Int -> Bool
prop_heap_size ts =
  heap_size == ranksum
  where
    heap_size = size ts
    ranksum = sum $ (2^) <$> (rank <$> ts)


-- a tree of rank r contains 2^r nodes
prop_heap_tree_sizes :: BinHeap Int -> Bool
prop_heap_tree_sizes ts =
  all correct_sized ts
  where
    correct_sized t = size [t] == 2^(rank t) -- treat each t as a [t] in order to use 'size'


-- minimum value of the heap is contained in a root node of one of the trees in heap
-- .. effectively no value in heap less than the min of roots of the heap's trees
prop_heap_min_elem_in_root :: BinHeap Int -> Bool
prop_heap_min_elem_in_root ts =
  minimum rootelems <= minimum remainders
  where
    rootelems = value <$> ts
    allelems = toList ts
    remainders = allelems \\ rootelems


-- heap accepts duplicate entries
prop_heap_accepts_dups :: Int -> Int -> Property -- n.b. Property, not Bool
prop_heap_accepts_dups n m = n > 0 ==>           -- return a Property when applying constraints!
  heapsize == length dups
  where
    heapsize = size $ fromList $ dups
    dups = replicate n m  -- n has to be positive


-- heap sorts entries properly
prop_heap_sorts :: [Int] -> Bool
prop_heap_sorts xs = binSort xs == sort xs
