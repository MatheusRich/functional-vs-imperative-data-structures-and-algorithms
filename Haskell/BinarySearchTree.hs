module BinarySearchTree where

import Prelude hiding ( map )

data BinarySearchTree a
  = Node a (BinarySearchTree a) (BinarySearchTree a)
  | Empty

new :: a -> BinarySearchTree a
new value = Node value Empty Empty

isEmpty :: BinarySearchTree a -> Bool
isEmpty Empty = True
isEmpty _     = False

push :: Ord a => a -> BinarySearchTree a -> BinarySearchTree a
push x Empty               = new x
push x (Node y left right) = if x > y
  then Node y left (push x right)
  else if x < y then Node y (push x left) right else (Node y left right)

find :: (a -> Bool) -> BinarySearchTree a -> Maybe a
find fn Empty               = Nothing
find fn (Node n left right) = if fn n
  then Just n
  else case find fn left of
    Nothing -> find fn right
    found   -> found

contains :: Ord a => a -> BinarySearchTree a -> Bool
contains target Empty                   = False
contains target (Node value left right) = if target < value
  then contains target left
  else if target > value then contains target right else True

depth :: BinarySearchTree a -> Int
depth Empty               = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

map :: (a -> b) -> BinarySearchTree a -> BinarySearchTree b
map fn Empty               = Empty
map fn (Node v left right) = Node (fn v) (map fn left) (map fn right)
