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


sortSuite list1 list2 list3 list4 list5 list6 list7 list8 =
    Benchmark.describe "Sort"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.mergeSort list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.mergeSort list2)
        , Benchmark.benchmark "500 elements" (\_ -> LL.mergeSort list3)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.mergeSort list4)
        , Benchmark.benchmark "2500 elements" (\_ -> LL.mergeSort list5)
        , Benchmark.benchmark "5000 elements" (\_ -> LL.mergeSort list6)
        , Benchmark.benchmark "7500 elements" (\_ -> LL.mergeSort list7)
        , Benchmark.benchmark "10000 elements" (\_ -> LL.mergeSort list8)
        ]


filterSuite list1 list2 list3 list4 list5 list6 list7 list8 =
    Benchmark.describe "Filter"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.filter isOdd list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.filter isOdd list2)
        , Benchmark.benchmark "500 elements" (\_ -> LL.filter isOdd list3)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.filter isOdd list4)
        , Benchmark.benchmark "2500 elements" (\_ -> LL.filter isOdd list5)
        , Benchmark.benchmark "5000 elements" (\_ -> LL.filter isOdd list6)
        , Benchmark.benchmark "7500 elements" (\_ -> LL.filter isOdd list7)
        , Benchmark.benchmark "10000 elements" (\_ -> LL.filter isOdd list8)
        ]


reduceSuite list1 list2 list3 list4 list5 list6 list7 list8 =
    Benchmark.describe "Reduce"
        [ Benchmark.benchmark "10 elements" (\_ -> LL.reduce (+) 0 list1)
        , Benchmark.benchmark "100 elements" (\_ -> LL.reduce (+) 0 list2)
        , Benchmark.benchmark "500 elements" (\_ -> LL.reduce (+) 0 list3)
        , Benchmark.benchmark "1000 elements" (\_ -> LL.reduce (+) 0 list4)
        , Benchmark.benchmark "2500 elements" (\_ -> LL.reduce (+) 0 list5)
        , Benchmark.benchmark "5000 elements" (\_ -> LL.reduce (+) 0 list6)
        , Benchmark.benchmark "7500 elements" (\_ -> LL.reduce (+) 0 list7)
        , Benchmark.benchmark "10000 elements" (\_ -> LL.reduce (+) 0 list8)
        ]


main : BenchmarkProgram
main =
    let
        list1 =
            createList 0 10 LL.Empty

        list2 =
            createList 0 100 LL.Empty

        list3 =
            createList 0 500 LL.Empty

        list4 =
            createList 0 1000 LL.Empty

        list5 =
            createList 0 2500 LL.Empty
        list6 =
            createList 0 5000 LL.Empty
        list7 =
            createList 0 7500 LL.Empty
        list8 =
            createList 0 10000 LL.Empty
    in
    program <|
        Benchmark.describe "Linked List"
            [ sortSuite list1 list2 list3 list4 list5 list6 list7 list8
            , filterSuite list1 list2 list3 list4 list5 list6 list7 list8
            , reduceSuite list1 list2 list3 list4 list5 list6 list7 list8
            ]
