module BinaryTree exposing (BinaryTree, contains, depth, first, insert, isEmpty, map, new)


type BinaryTree a
    = Empty
    | Node a (BinaryTree a) (BinaryTree a)


new : a -> BinaryTree a
new value =
    Node value Empty Empty


isEmpty : BinaryTree a -> Bool
isEmpty tree =
    case tree of
        Empty ->
            True

        _ ->
            False


insert : BinaryTree comparable -> comparable -> BinaryTree comparable
insert tree x =
    case tree of
        Empty ->
            new x

        Node y left right ->
            if x > y then
                Node y left (insert right x)

            else if x < y then
                Node y (insert left x) right

            else
                tree


contains : BinaryTree comparable -> comparable -> Bool
contains tree target =
    case tree of
        Empty ->
            False

        Node value left right ->
            if target < value then
                contains left target

            else if target > value then
                contains right target

            else
                True


depth : BinaryTree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node _ left right ->
            1 + max (depth left) (depth right)


map : BinaryTree a -> (a -> b) -> BinaryTree b
map tree func =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (func v) (map left func) (map right func)


first : BinaryTree a -> Maybe a
first tree =
    case tree of
        Empty ->
            Nothing

        Node v _ _ ->
            Just v
