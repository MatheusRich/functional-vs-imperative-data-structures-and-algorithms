module LinkedList where

import Prelude hiding (foldl, head, last, length, reverse, tail)

data LinkedList a
  = Value a (LinkedList a)
  | Empty
  deriving (Show)

new :: a -> LinkedList a
new value =
  Value value Empty

isEmpty :: LinkedList a -> Bool
isEmpty Empty = True
isEmpty _ = False

head :: LinkedList a -> Maybe a
head Empty = Nothing
head (Value head _) = Just head

tail :: LinkedList a -> LinkedList a
tail (Value _ tail) = tail
tail Empty = Empty

push :: a -> LinkedList a -> LinkedList a
push value list = Value value list

index :: Integer -> LinkedList a -> Maybe a
index 0 list = head list
index idx list =
  if idx < 0
    then Nothing
    else index (idx - 1) (LinkedList.tail list)

length :: LinkedList a -> Integer
length Empty = 0
length list = 1 + (length (tail list))

reverse :: LinkedList a -> LinkedList a
reverse Empty = Empty
reverse (Value listHead listTail) = push (listHead) (reverse listTail)

last :: LinkedList a -> Maybe a
last Empty = Nothing
last (Value listHead Empty) = Just listHead
last list = last (tail list)
