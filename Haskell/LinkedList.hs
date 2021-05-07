module LinkedList where

import Prelude hiding (filter, head, last, length, map, reverse, tail, drop, take)

data LinkedList a
  = Value a (LinkedList a)
  | Empty

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
length list = lengthAcc list 0
  where
    lengthAcc Empty acc = acc
    lengthAcc (Value listHead listTail) acc = lengthAcc listTail (1 + acc)

reverse :: LinkedList a -> LinkedList a
reverse list = reverseAcc list Empty
  where
    reverseAcc Empty acc = acc
    reverseAcc (Value listHead listTail) acc = reverseAcc listTail (push listHead acc)

last :: LinkedList a -> Maybe a
last Empty = Nothing
last (Value listHead Empty) = Just listHead
last list = last (tail list)

map :: (a -> b) -> LinkedList a -> LinkedList b
map _ Empty = Empty
map fn (Value listHead listTail) = (Value (fn listHead)) (map fn listTail)

filter :: (a -> Bool) -> LinkedList a -> LinkedList a
filter _ Empty = Empty
filter fn (Value listHead listTail) =
  if fn (listHead)
    then Value listHead (filter fn listTail)
    else filter fn listTail

reduce :: (a -> a -> a) -> a -> LinkedList a -> a
reduce _ acc Empty = acc
reduce fn acc (Value listHead listTail) =
  reduce fn (fn listHead acc) (listTail)

mergeSort :: (Ord a) => LinkedList a -> LinkedList a
mergeSort Empty = Empty
mergeSort (Value a Empty) = (Value a Empty)
mergeSort list = merge (mergeSort left) (mergeSort right)
  where
    left = take ((length list) `div` 2) list
    right = drop ((length list) `div` 2) list

merge :: (Ord a) => LinkedList a -> LinkedList a -> LinkedList a
merge left Empty = left
merge Empty right = right
merge (Value head1 tail1) (Value head2 tail2) =
  if head1 < head2
    then push head1 (merge tail1 right)
    else push head2 (merge left tail2)
    where
      left = (Value head1 tail1)
      right = (Value head2 tail2)

take :: Integer -> LinkedList a -> LinkedList a
take _ Empty = Empty
take 0 list = list
take 1 (Value listHead _) = new listHead
take n (Value listHead listTail) = Value listHead (take (n - 1) listTail)

drop :: Integer -> LinkedList a -> LinkedList a
drop _ Empty = Empty
drop 0 list = list
drop n (Value _ listTail) = drop (n - 1) listTail
