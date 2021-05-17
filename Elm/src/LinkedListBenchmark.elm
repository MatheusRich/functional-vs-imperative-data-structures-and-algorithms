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
            createList (begin + 1) (end - 1) (LL.push (begin + 1) list)


isOdd x =
    remainderBy 2 x == 1


sortSuite list1 list2 list3 list4 =
    Benchmark.describe "Sort"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.mergeSort list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.mergeSort list2)
        , Benchmark.benchmark "1 000 elements" (\_ -> LL.mergeSort list3)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.mergeSort list4)
        ]


filterSuite list1 list2 list3 list4 =
    Benchmark.describe "Filter"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.filter isOdd list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.filter isOdd list2)
        , Benchmark.benchmark "1 000 elements" (\_ -> LL.filter isOdd list3)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.filter isOdd list4)
        ]


reduceSuite list1 list2 list3 list4 =
    Benchmark.describe "Reduce"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.reduce (+) 0 list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.reduce (+) 0 list2)
        , Benchmark.benchmark "1 000 elements" (\_ -> LL.reduce (+) 0 list3)
        , Benchmark.benchmark "10 000 elements" (\_ -> LL.reduce (+) 0 list4)
        ]


main : BenchmarkProgram
main =
    let
        list1 =
            createList 0 10 LL.Empty

        list2 =
            createList 0 100 LL.Empty

        list3 =
            createList 0 1000 LL.Empty

        list4 =
            createList 0 10000 LL.Empty
    in
    program <|
        Benchmark.describe "Linked List"
            [ sortSuite list1 list2 list3 list4
            , filterSuite list1 list2 list3 list4
            , reduceSuite list1 list2 list3 list4
            ]
