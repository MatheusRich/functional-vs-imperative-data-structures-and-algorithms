module BSTTest exposing (main)

import BinarySearchTree as BST exposing (BinarySearchTree(..))
import Headless exposing (..)


main : Program Headless.Flags Headless.Model Headless.Msg
main =
    Headless.run binarySearchTreeTest


binarySearchTreeTest : InputType -> OutputType
binarySearchTreeTest i =
    let
        emptyTree =
            BST.Empty

        tree =
            BST.push 5 (BST.push -4 (BST.push -1 (BST.new 3)))

        firstNegative =
            case BST.find (\x -> x < 0) tree of
                Just value ->
                    value

                Nothing ->
                    0
    in
    case i of
        1 ->
            "Creating a new binary search tree of integers"

        2 ->
            if BST.isEmpty emptyTree then
                "It is empty"

            else
                "fails"

        3 ->
            "Pushing 3, -1, -4 and 5"

        4 ->
            if not (BST.isEmpty tree) then
                "It is not empty"

            else
                "fails"

        5 ->
            "Depth is " ++ String.fromInt (BST.depth tree)

        6 ->
            "First negative number is " ++ String.fromInt firstNegative

        _ ->
            "fails"
