module BinarySearchTree exposing (BinarySearchTree(..), contains, depth, find, isEmpty, map, new, push)


type BinarySearchTree a
    = Empty
    | Node a (BinarySearchTree a) (BinarySearchTree a)


new : a -> BinarySearchTree a
new value =
    Node value Empty Empty


isEmpty : BinarySearchTree a -> Bool
isEmpty tree =
    case tree of
        Empty ->
            True

        _ ->
            False


push : comparable -> BinarySearchTree comparable -> BinarySearchTree comparable
push x tree =
    case tree of
        Empty ->
            new x

        Node y left right ->
            if x > y then
                Node y left (push x right)

            else if x < y then
                Node y (push x left) right

            else
                tree


find : (a -> Bool) -> BinarySearchTree a -> Maybe a
find fn tree =
    case tree of
        Empty ->
            Nothing

        Node n left right ->
            if fn n then
                Just n

            else
                case find fn left of
                    Nothing ->
                        find fn right

                    found ->
                        found


contains : comparable -> BinarySearchTree comparable -> Bool
contains target tree =
    case tree of
        Empty ->
            False

        Node value left right ->
            if target < value then
                contains target left

            else if target > value then
                contains target right

            else
                True


depth : BinarySearchTree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node _ left right ->
            1 + max (depth left) (depth right)


map : (a -> b) -> BinarySearchTree a -> BinarySearchTree b
map fn tree =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (fn v) (map fn left) (map fn right)

