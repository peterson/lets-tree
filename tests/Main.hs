module Main (main) where

import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)


-- import BinaryTree as BT
import           BinomialHeap                         as Binom

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        -- testGroup "Binary Tree" [
        --         testProperty "balanced" BT.prop_balanced,
        --         testProperty "sort" BT.prop_sort
        --         testProperty "sort2" prop_sort2,
        --         testCase "sort3" test_sort3
        --     ],
        testGroup "Binomial Heap" [
                testProperty "has one tree per rank" Binom.prop_heap_one_tree_per_rank,
                testProperty "has correct number of trees" Binom.prop_heap_max_trees,
                testProperty "has correct heap size" Binom.prop_heap_size,
                testProperty "has correct tree sizes" Binom.prop_heap_tree_sizes,
                testProperty "accepts duplicates" Binom.prop_heap_accepts_dups,
                testProperty "sorts properly" Binom.prop_heap_sorts
            ]
    ]
