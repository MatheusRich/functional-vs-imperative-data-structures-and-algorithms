module LinkedList exposing (..)

type LinkedList a = Cons a (LinkedList a) | Nothing

-- CREATE

push : LinkedList a -> Maybe a -> LinkedList a
push list value =
    case value of
        Maybe.Nothing ->
            list
        Just v ->
            Cons v list


-- TRANSFORM
-- map : LinkedList a -> (a -> b) -> LinkedList b
-- filter : LinkedList a -> (a -> Bool) -> LinkedList a

-- DECONSTRUCT

isEmpty : LinkedList a -> Bool
isEmpty list =
    case list of
        Nothing ->
            True
        _ ->
          False


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


index : LinkedList a -> Int -> Maybe a
index list idx =
    case (list, idx) of
        (l, 0) ->
            head l
        (l, i) ->
            index (tail l) (i-1)


last : LinkedList a -> Maybe a
last list =
    case list of
        Nothing ->
            Maybe.Nothing
        Cons a Nothing ->
            Just a
        _ ->
            last (tail list)

-- UTILITIES

length : LinkedList a -> Int
length list =
  case list of
        Nothing ->
            0
        notEmptyList ->
            1 + length (tail notEmptyList)


-- COMBINE

append : LinkedList a -> LinkedList a -> LinkedList a
append list1 list2 =
    case (list1, list2) of
        (Nothing, l2) ->
            l2
        ((Cons a Nothing), l2) ->
            Cons a l2
        (l1, l2) ->
            push (append (tail l1) l2) (head l1)


-- SORT

-- sort : LinkedList comparable -> LinkedList comparable



-- l = (Cons 1 (LinkedList.Nothing))
-- LinkedList.isEmpty l
-- l2 = LinkedList.push l (Just 2)
-- LinkedList.tail l2