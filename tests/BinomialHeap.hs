module BinomialHeap where

import           Data.List         (sort)
import           Lets.BinomialHeap

prop_sort xs = binSort xs == sort xs
  where types = (xs :: [Int])
