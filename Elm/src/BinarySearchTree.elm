module BinarySearchTree exposing (BinarySearchTree, contains, depth, find, first, isEmpty, map, new, push)


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


push : BinarySearchTree comparable -> comparable -> BinarySearchTree comparable
push tree x =
    case tree of
        Empty ->
            new x

        Node y left right ->
            if x > y then
                Node y left (push right x)

            else if x < y then
                Node y (push left x) right

            else
                tree


find : BinarySearchTree a -> (a -> Bool) -> Maybe a
find tree fn =
    case tree of
        Empty ->
            Nothing

        Node n left right ->
            if fn n then
                Just n

            else
                case find left fn of
                    Nothing ->
                        find right fn

                    found ->
                        found


contains : BinarySearchTree comparable -> comparable -> Bool
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


depth : BinarySearchTree a -> Int
depth tree =
    case tree of
        Empty ->
            0

        Node _ left right ->
            1 + max (depth left) (depth right)


map : BinarySearchTree a -> (a -> b) -> BinarySearchTree b
map tree func =
    case tree of
        Empty ->
            Empty

        Node v left right ->
            Node (func v) (map left func) (map right func)


first : BinarySearchTree a -> Maybe a
first tree =
    case tree of
        Empty ->
            Nothing

        Node v _ _ ->
            Just v
