module Main (main) where

import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)


import BinaryTree as BinTree
import           BinomialHeap                         as Binom

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        testGroup "Binary Tree" [
                testProperty "is ordered" BinTree.prop_bintree_ordered
              , testProperty "is balanced" BinTree.prop_bintree_balanced
              -- , testProperty "has minimum height" BinTree.prop_bintree_minimumheight
            ],
        testGroup "Binomial Heap" [
                testProperty "is a minimum heap" Binom.prop_heap_is_minimum
              , testProperty "has one tree per rank" Binom.prop_heap_one_tree_per_rank
              , testProperty "has correct number of trees" Binom.prop_heap_max_trees
              , testProperty "has correct heap size" Binom.prop_heap_size
              , testProperty "has correct tree sizes" Binom.prop_heap_tree_sizes
              , testProperty "accepts duplicates" Binom.prop_heap_accepts_dups
              , testProperty "sorts properly" Binom.prop_heap_sorts
            ]
    ]
