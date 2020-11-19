module LinkedListTest exposing (main)

import Headless exposing (..)
import LinkedList exposing (LinkedList(..))


main : Program Headless.Flags Headless.Model Headless.Msg
main =
    Headless.run linkedListTest


linkedListTest : InputType -> OutputType
linkedListTest i =
    let
        emptyList =
            LinkedList.Empty

        list =
            Value 5 (Value 1 (Value 4 (Value 1 (Value 3 Empty))))

        listHead =
            case LinkedList.head list of
                Just value ->
                    value

                Nothing ->
                    -999

        listLast =
            case LinkedList.last list of
                Just value ->
                    value

                Nothing ->
                    -999
    in
    case i of
        1 ->
            "Creating a new linked list of integers"

        2 ->
            if LinkedList.isEmpty emptyList then
                "It is empty"

            else
                "fails"

        3 ->
            "Pushing 3, 1, 4, 1, and 5"

        4 ->
            if not (LinkedList.isEmpty list) then
                "It is not empty"

            else
                "fails"

        5 ->
            "Length is " ++ String.fromInt (LinkedList.length list)

        6 ->
            "List is " ++ LinkedList.toString list

        7 ->
            "Head is " ++ String.fromInt listHead

        8 ->
            "Tail is " ++ LinkedList.toString (LinkedList.tail list)

        9 ->
            "Tail length is " ++ String.fromInt (LinkedList.length (LinkedList.tail list))

        10 ->
            "Last is " ++ String.fromInt listLast

        11 ->
            "Reversed list is " ++ LinkedList.toString (LinkedList.reverse list)

        12 ->
            "List as string is " ++ LinkedList.toString2 (LinkedList.map (\x -> String.fromInt x) list)

        13 ->
            "List without even numbers is " ++ LinkedList.toString (LinkedList.filter (\x -> remainderBy 2 x == 1) list)

        14 ->
            "Sum of all list elements is " ++ String.fromInt (LinkedList.reduce (+) 0 list)

        15 ->
            "Sorted list is " ++ LinkedList.toString (LinkedList.sort list)

        _ ->
            "fails"
