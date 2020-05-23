module LinkedList exposing (..)

type LinkedList a = Cons a (LinkedList a) | Nothing

push : LinkedList a -> a -> LinkedList a
push list value =
    Cons value list

head : LinkedList a -> Maybe a
head list =
    case list of
        Cons a _ ->
            Just a
        Nothing ->
            Maybe.Nothing


tail : LinkedList a -> LinkedList a
tail list =
  case list of
    Cons _ a ->
        a
    Nothing ->
        Nothing


last : LinkedList a -> Maybe a
last list =
    case list of
        Nothing ->
            Maybe.Nothing
        Cons a Nothing ->
            Just a
        _ ->
            last (tail list)


length : LinkedList a -> Int
length list =
  case list of
        Nothing ->
            0
        notEmptyList ->
            1 + length (tail notEmptyList)


isEmpty : LinkedList a -> Bool
isEmpty list =
    case list of
        Nothing ->
            True
        _ ->
          False

-- l = (Cons 1 (LinkedList.Nothing))
-- LinkedList.isEmpty l
-- l2 = LinkedList.push l 2
-- LinkedList.tail l2