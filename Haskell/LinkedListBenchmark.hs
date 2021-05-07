import Criterion.Main
import qualified LinkedList as LL

createList :: Int -> Int -> LL.LinkedList Int -> LL.LinkedList Int
createList begin 0   list = list
createList begin end list = createList (begin + 1) (end - 1) (LL.push begin list)

benchmark :: LL.LinkedList Int -> LL.LinkedList Int -> LL.LinkedList Int -> IO ()
benchmark list1 list2 list3 = defaultMain
  [ bgroup
    "Linked List Sort"
    [ bench "10 000 elements" $ whnf LL.mergeSort list1
    , bench "100 000 elements" $ whnf LL.mergeSort list2
    , bench "1 000 000 elements" $ whnf LL.mergeSort list3
    ]
  , bgroup
    "Linked List Filter"
    [ bench "10 000 elements" $ whnf (LL.filter (\x -> rem x 2 /= 0)) list1
    , bench "100 000 elements" $ whnf (LL.filter (\x -> rem x 2 /= 0)) list2
    , bench "1 000 000 elements" $ whnf (LL.filter (\x -> rem x 2 /= 0)) list3
    ]
  , bgroup
    "Linked List Reduce"
    [ bench "10 000 elements" $ whnf (LL.reduce (+) 0) list1
    , bench "100 000 elements" $ whnf (LL.reduce (+) 0) list2
    , bench "1 000 000 elements" $ whnf (LL.reduce (+) 0) list3
    ]
  ]

main = do
  putStrLn "Bench for 10 000, 100 000 and 1 000 000 elements"
  let list  = createList 0 10000 LL.Empty
  let list2 = createList 0 100000 LL.Empty
  let list3 = createList 0 1000000 LL.Empty
  benchmark list list2 list3
