module BSTBenchmark exposing (main)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BinarySearchTree as BST


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


bstFromList : List Int -> BST.BinarySearchTree Int
bstFromList list =
    let
        ( half0, half2 ) =
            splitAt (List.length list // 2) list

        half1 =
            case List.tail half2 of
                Nothing ->
                    []

                Just something ->
                    something

        mid =
            case List.head half2 of
                Nothing ->
                    -1

                Just something ->
                    something
    in
    case list of
        [] ->
            BST.Empty

        _ ->
            BST.Node mid (bstFromList half0) (bstFromList half1)


containsSuite tree1 tree2 tree3 tree4 tree5 tree6 tree7 tree8 =
    Benchmark.describe "Contains"
        [ Benchmark.benchmark "with 10 elements" (\_ -> BST.contains 10 tree1)
        , Benchmark.benchmark "with 100 elements" (\_ -> BST.contains 100 tree2)
        , Benchmark.benchmark "with 500 elements" (\_ -> BST.contains 500 tree3)
        , Benchmark.benchmark "with 1000 elements" (\_ -> BST.contains 1000 tree4)
        , Benchmark.benchmark "with 2500 elements" (\_ -> BST.contains 2500 tree5)
        , Benchmark.benchmark "with 5000 elements" (\_ -> BST.contains 5000 tree6)
        , Benchmark.benchmark "with 7500 elements" (\_ -> BST.contains 7500 tree7)
        , Benchmark.benchmark "with 10000 elements" (\_ -> BST.contains 10000 tree8)
        ]


mapSuite tree1 tree2 tree3 tree4 tree5 tree6 tree7 tree8 =
    let
        doubleN x =
            x * 2
    in
    Benchmark.describe "Map"
        [ Benchmark.benchmark "with 10 elements" (\_ -> BST.map doubleN tree1)
        , Benchmark.benchmark "with 100 elements" (\_ -> BST.map doubleN tree2)
        , Benchmark.benchmark "with 500 elements" (\_ -> BST.map doubleN tree3)
        , Benchmark.benchmark "with 1000 elements" (\_ -> BST.map doubleN tree4)
        , Benchmark.benchmark "with 2500 elements" (\_ -> BST.map doubleN tree5)
        , Benchmark.benchmark "with 5000 elements" (\_ -> BST.map doubleN tree6)
        , Benchmark.benchmark "with 7500 elements" (\_ -> BST.map doubleN tree7)
        , Benchmark.benchmark "with 10 000 elements" (\_ -> BST.map doubleN tree8)
        ]


main : BenchmarkProgram
main =
    let
        tree1 =
            bstFromList (List.range 1 10)

        tree2 =
            bstFromList (List.range 1 100)

        tree3 =
            bstFromList (List.range 1 500)

        tree4 =
            bstFromList (List.range 1 1000)

        tree5 =
            bstFromList (List.range 1 2500)

        tree6 =
            bstFromList (List.range 1 5000)

        tree7 =
            bstFromList (List.range 1 7500)

        tree8 =
            bstFromList (List.range 1 10000)
    in
    program <|
        Benchmark.describe "Binary Search Tree"
            [ containsSuite tree1 tree2 tree3 tree4 tree5 tree6 tree7 tree8
            , mapSuite tree1 tree2 tree3 tree4 tree5 tree6 tree7 tree8
            ]
