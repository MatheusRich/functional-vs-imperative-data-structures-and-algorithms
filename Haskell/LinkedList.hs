module LinkedList where

import Prelude hiding (head, last, length, reverse, tail)

data LinkedList a
  = Value a (LinkedList a)
  | Empty
  deriving (Show, Eq)

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

toString :: (Show a) => LinkedList a -> String
toString list = toStringAcc list ""
  where
    toStringAcc Empty acc = acc ++ "X"
    toStringAcc (Value listHead listTail) acc = toStringAcc listTail (acc ++ show (listHead) ++ " -> ")
