module LinkedList exposing
    ( LinkedList(..)
    , append
    , drop
    , foldl
    , head
    , index
    , isEmpty
    , last
    , length
    , map
    , merge
    , mergeSort
    , new
    , push
    , reverse
    , sort
    , tail
    , take
    )


type LinkedList a
    = Value a (LinkedList a)
    | Empty



-- CREATE


new : a -> LinkedList a
new value =
    Value value Empty


push : LinkedList a -> Maybe a -> LinkedList a
push list value =
    case value of
        Nothing ->
            list

        Just v ->
            Value v list



-- TRANSFORM


map : LinkedList a -> (a -> b) -> LinkedList b
map list fn =
    case list of
        Empty ->
            Empty

        Value listHead Empty ->
            new (fn listHead)

        Value listHead listTail ->
            push (map listTail fn) (Just (fn listHead))


reverse : LinkedList a -> LinkedList a
reverse list =
    foldl Value Empty list


foldl : (a -> b -> b) -> b -> LinkedList a -> b
foldl func acc list =
    case list of
        Empty ->
            acc

        Value listHead listTail ->
            foldl func (func listHead acc) listTail



-- filter : LinkedList a -> (a -> Bool) -> LinkedList a
-- SORT


sort : LinkedList comparable -> LinkedList comparable
sort list =
    mergeSort list


mergeSort : LinkedList comparable -> LinkedList comparable
mergeSort list =
    case list of
        Empty ->
            Empty

        Value _ Empty ->
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
        ( _, Empty ) ->
            list1

        ( Empty, _ ) ->
            list2

        ( Value head1 tail1, Value head2 tail2 ) ->
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
            Empty ->
                list

            Value _ listTail ->
                drop listTail (n - 1)


take : LinkedList a -> Int -> LinkedList a
take list n =
    if n <= 0 then
        list

    else
        case ( list, n ) of
            ( Empty, _ ) ->
                list

            ( Value listHead _, 1 ) ->
                new listHead

            ( Value listHead listTail, _ ) ->
                push (take listTail (n - 1)) (Just listHead)



-- DECONSTRUCT


isEmpty : LinkedList a -> Bool
isEmpty list =
    case list of
        Empty ->
            True

        _ ->
            False


head : LinkedList a -> Maybe a
head list =
    case list of
        Empty ->
            Nothing

        Value a _ ->
            Just a


tail : LinkedList a -> LinkedList a
tail list =
    case list of
        Value _ a ->
            a

        Empty ->
            Empty


index : LinkedList a -> Int -> Maybe a
index list idx =
    if idx < 0 then
        Nothing

    else
        case idx of
            0 ->
                head list

            _ ->
                index (tail list) (idx - 1)


last : LinkedList a -> Maybe a
last list =
    case list of
        Empty ->
            Nothing

        Value listHead Empty ->
            Just listHead

        _ ->
            last (tail list)



-- UTILITIES


length : LinkedList a -> Int
length list =
    case list of
        Empty ->
            0

        notEmptyList ->
            1 + length (tail notEmptyList)



-- COMBINE


append : LinkedList a -> LinkedList a -> LinkedList a
append list1 list2 =
    case ( list1, list2 ) of
        ( Empty, l2 ) ->
            l2

        ( Value listHead Empty, l2 ) ->
            Value listHead l2

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
