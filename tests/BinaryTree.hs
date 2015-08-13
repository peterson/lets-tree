{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BinaryTree where

import           Control.Monad     (liftM)
import           Data.List         (nub, sort, (\\))
import           Lets.BinaryTree
import           Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = liftM fromList arbitrary

-- value of left child <= value of root <= value of right child
prop_bintree_ordered :: Tree Int -> Bool
prop_bintree_ordered = ordered -- defined in Lets.BinaryTree


-- left and right sub-trees of each node differ in height by no more than 1
prop_bintree_balanced :: Tree Int -> Bool
prop_bintree_balanced = balanced -- defined in Lets.BinaryTree
