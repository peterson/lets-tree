module Main (main) where

import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)


-- import BinaryTree as BT
import           BinomialHeap                         as BH

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
        -- testGroup "Binary Tree" [
        --         testProperty "balanced" BT.prop_balanced,
        --         testProperty "sort" BT.prop_sort
        --     ],
        testGroup "Binomial Heap" [
                testProperty "sort" BH.prop_sort
                -- testProperty "sort2" prop_sort2,
                -- testCase "sort3" test_sort3
            ]
    ]
