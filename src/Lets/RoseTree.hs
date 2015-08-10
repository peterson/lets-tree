{-

Lets.RoseTree

A Rose Tree (or multi-way tree) is a tree structure with a variable and unbounded
number of branches at each node. This implementation allows for an empty tree.

References

[1] Wikipedia, "Rose Tree", http://en.wikipedia.org/wiki/Rose_tree

[2] Jeremy Gibbons, "Origami Programming", appears in The Fun of Programming
    edited by J. Gibbons and O. de Moor, Palgrave McMillan, 2003, pages 41-60.
    PDF available at: http://www.staff.science.uu.nl/~3860418/msc/11_infomtpt/papers/origami-programming_Gibbons.pdf

-}


module Lets.RoseTree
  ( RoseTree

  -- constructors
  , node
  , singleton

  -- properties
  , value
  , children
  , forest
  , size
  , depth

  -- operations
  , insert
  , toList
  , flatten

  )
  where

--
-- definitions
--

data RoseTree a = Empty
                | Node a (Forest a)
                deriving (Show, Eq)

type Forest a = [RoseTree a]

--
-- constructors
--

node :: a -> [RoseTree a] -> RoseTree a
node v t = Node v t

singleton :: a -> RoseTree a
singleton v = Node v []


--
-- operations
--

value :: RoseTree a -> Maybe a
value (Empty)    = Nothing
value (Node v _) = Just v

children :: RoseTree a -> [RoseTree a]
children (Empty)    = []
children (Node _ t) = t -- [] or [...]
-- also called forest
forest = children

size :: RoseTree a -> Int
size (Empty)     = 0
size (Node v ts) = 1 + sum (map size ts)

depth :: RoseTree a -> Int
depth (Empty)     = 0
depth (Node _ []) = 1
depth (Node v ts) = 1 + maximum (map depth ts)

toList :: RoseTree a -> [a]
toList Empty = []
toList (Node v t) = v : concatMap toList t
-- also called flatten
flatten = toList

--
-- a simple insert operation that places tree q at the head of the children
-- of tree r
insert :: RoseTree a -> RoseTree a -> RoseTree a
insert Empty r = r
insert q Empty = q
insert q r@(Node v t) = Node v (q:t)


--
-- sample
--

rt1 = singleton 1
rt2 = singleton 2
rt3 = node 3 [rt1, rt2]
rt4 = node 8 [node 3 [], node 4 []]
rt5 = node 12 [rt4, rt3]
