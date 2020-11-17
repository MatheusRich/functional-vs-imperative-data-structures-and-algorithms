module LinkedList exposing
    ( LinkedList(..)
    , drop
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


push : a -> LinkedList a -> LinkedList a
push value list =
    Value value list



-- TRANSFORM


map : (a -> b) -> LinkedList a -> LinkedList b
map fn list =
    case list of
        Empty ->
            Empty

        Value listHead Empty ->
            new (fn listHead)

        Value listHead listTail ->
            push (fn listHead) (map fn listTail)


reverse : LinkedList a -> LinkedList a
reverse linkedList =
    let
        rev list acc =
            case ( list, acc ) of
                ( Empty, _ ) ->
                    acc

                ( Value listHead listTail, _ ) ->
                    rev listTail (push listHead acc)
    in
    rev linkedList Empty



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
                    take (length list // 2) list

                right =
                    drop (length list // 2) list
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
                push head1 (merge tail1 list2)

            else
                push head2 (merge list1 tail2)


drop : Int -> LinkedList a -> LinkedList a
drop n list =
    if n <= 0 then
        list

    else
        case list of
            Empty ->
                list

            Value _ listTail ->
                drop (n - 1) listTail


take : Int -> LinkedList a -> LinkedList a
take n list =
    if n <= 0 then
        list

    else
        case ( list, n ) of
            ( Empty, _ ) ->
                list

            ( Value listHead _, 1 ) ->
                new listHead

            ( Value listHead listTail, _ ) ->
                push listHead (take (n - 1) listTail)



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


index : Int -> LinkedList a -> Maybe a
index idx list =
    if idx < 0 then
        Nothing

    else
        case idx of
            0 ->
                head list

            _ ->
                index (idx - 1) (tail list)


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



-- UTILS


double : Int -> Int
double n =
    n * 2


divide : Int -> Float
divide n =
    toFloat n / 2



-- l = LinkedList.Empty
-- LinkedList.isEmpty l
-- l2 = LinkedList.push l (Just 2)
-- LinkedList.tail l2
