module LinkedListBenchmark exposing (main, sortSuite)

import Benchmark exposing (Benchmark)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import LinkedList as LL


createList : Int -> Int -> LL.LinkedList Int -> LL.LinkedList Int
createList begin end list =
    case end of
        0 ->
            list

        _ ->
            createList (begin + 1) (end - 1) (LL.push begin list)


sortSuite : LL.LinkedList Int -> LL.LinkedList Int -> LL.LinkedList Int -> Benchmark
sortSuite list1 list2 list3 =
    Benchmark.describe "Sort"
        [ Benchmark.benchmark "100 elements" (\_ -> LL.sort list1)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.sort list2)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.sort list3)
        ]


filterSuite : LL.LinkedList Int -> LL.LinkedList Int -> LL.LinkedList Int -> Benchmark
filterSuite list1 list2 list3 =
    Benchmark.describe "Filter"
        [ Benchmark.benchmark "100 elements" (\_ -> LL.filter (\x -> remainderBy 2 x == 1) list1)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.filter (\x -> remainderBy 2 x == 1) list2)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.filter (\x -> remainderBy 2 x == 1) list3)
        ]


reduceSuite : LL.LinkedList Int -> LL.LinkedList Int -> LL.LinkedList Int -> Benchmark
reduceSuite list1 list2 list3 =
    Benchmark.describe "Reduce"
        [ Benchmark.benchmark "100 elements" (\_ -> LL.reduce (+) 0 list1)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.reduce (+) 0 list2)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.reduce (+) 0 list3)
        ]


main : BenchmarkProgram
main =
    let
        list1 =
            createList 0 100 LL.Empty

        list2 =
            createList 0 1000 LL.Empty

        list3 =
            createList 0 10000 LL.Empty
    in
    program <|
        Benchmark.describe "Linked List +"
            [ sortSuite list1 list2 list3
            , filterSuite list1 list2 list3
            , reduceSuite list1 list2 list3
            ]
