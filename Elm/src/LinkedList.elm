module LinkedList exposing (..)


type LinkedList a
    = Cons a (LinkedList a)
    | Nothing



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
-- SORT


mergeSort : LinkedList comparable -> LinkedList comparable
mergeSort list =
    case list of
        Nothing ->
            Nothing

        Cons _ Nothing ->
            list

        _ ->
            let
                left =
                    take list (length list // 2)

                right =
                    drop list (length list // 2)
            in
            merge (mergeSort left) (mergeSort right)


merge : LinkedList comparable -> LinkedList comparable -> LinkedList comparable
merge list1 list2 =
    case ( list1, list2 ) of
        ( _, Nothing ) ->
            list1

        ( Nothing, _ ) ->
            list2

        ( Cons head1 tail1, Cons head2 tail2 ) ->
            if head1 < head2 then
                push (merge tail1 list2) (Just head1)

            else
                push (merge list1 tail2) (Just head2)


drop : LinkedList a -> Int -> LinkedList a
drop list n =
    if n <= 0 then
        list

    else
        case list of
            Nothing ->
                list

            Cons _ listTail ->
                drop listTail (n - 1)


take : LinkedList a -> Int -> LinkedList a
take list n =
    if n <= 0 then
        list

    else
        case ( list, n ) of
            ( Nothing, _ ) ->
                list

            ( Cons listHead _, 1 ) ->
                new listHead

            ( Cons listHead listTail, _ ) ->
                push (take listTail (n - 1)) (Just listHead)



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
    case ( list1, list2 ) of
        ( Nothing, l2 ) ->
            l2

        ( Cons listHead Nothing, l2 ) ->
            Cons listHead l2

        ( l1, l2 ) ->
            push (append (tail l1) l2) (head l1)


-- UTILS


double : Int -> Int
double n =
    n * 2


divide : Int -> Float
divide n =
    toFloat n / 2



-- l = (Cons 1 (LinkedList.Nothing))
-- LinkedList.isEmpty l
-- l2 = LinkedList.push l (Just 2)
-- LinkedList.tail l2
