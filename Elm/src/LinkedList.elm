module LinkedList exposing (..)

type LinkedList a = Cons a (LinkedList a) | Nothing

-- CREATE

-- infix right 5 (::) = cons

new : a -> LinkedList a
new value =
    Cons value Nothing


push : LinkedList a -> Maybe a -> LinkedList a
push list value =
    case value of
        Maybe.Nothing ->
            list
        Just v ->
            Cons v list


-- TRANSFORM
map : LinkedList a -> (a -> b) -> LinkedList b
map list fn =
    case list of
        Nothing ->
            Nothing
        Cons listHead Nothing ->
            new (fn listHead)
        Cons listHead listTail ->
            push (map listTail fn) (Just (fn listHead))


reverse : LinkedList a -> LinkedList a
reverse list =
  foldl Cons Nothing list


foldl : (a -> b -> b) -> b -> LinkedList a -> b
foldl func acc list =
    case list of
        Nothing ->
            acc
        Cons listHead listTail ->
            foldl func (func listHead acc) listTail


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
        Nothing ->
            Maybe.Nothing
        Cons a _ ->
            Just a


tail : LinkedList a -> LinkedList a
tail list =
  case list of
    Cons _ a ->
        a
    Nothing ->
        Nothing


index : LinkedList a -> Int -> Maybe a
index list idx =
    if idx < 0 then
        Maybe.Nothing
    else
        case idx of
            0 ->
                head list
            _ ->
                index (tail list) (idx - 1)


last : LinkedList a -> Maybe a
last list =
    case list of
        Nothing ->
            Maybe.Nothing
        Cons listHead Nothing ->
            Just listHead
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
        ((Cons listHead Nothing), l2) ->
            Cons listHead l2
        (l1, l2) ->
            push (append (tail l1) l2) (head l1)


-- SORT

-- sort : LinkedList comparable -> LinkedList comparable


-- UTILS

double : (Int) -> Int
double n =
    n * 2   

divide : Int -> Float
divide n =
    toFloat (n) / 2   

-- l = (Cons 1 (LinkedList.Nothing))
-- LinkedList.isEmpty l
-- l2 = LinkedList.push l (Just 2)
-- LinkedList.tail l2