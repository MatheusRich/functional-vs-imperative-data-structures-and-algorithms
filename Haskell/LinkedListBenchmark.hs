{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)
import Control.DeepSeq
import Criterion.Main
import qualified LinkedList as LL

deriving instance Generic (LL.LinkedList a)
deriving instance NFData a => NFData (LL.LinkedList a)

createList :: Int -> Int -> LL.LinkedList Int -> LL.LinkedList Int
createList begin 0   list = list
createList begin end list = createList (begin + 1) (end - 1) (LL.push (begin + 1) list)

isOdd n = rem n 2 /= 0

benchmark list1 list2 list3 list4 list5 list6 list7 list8 list9 = defaultMain
  [ bgroup
    "Linked List Sort"
    [ bench "10 elements" $ whnf LL.mergeSort list1
    , bench "100 elements" $ whnf LL.mergeSort list2
    , bench "1000 elements" $ whnf LL.mergeSort list3
    , bench "10000 elements" $ whnf LL.mergeSort list4
    , bench "100000 elements" $ whnf LL.mergeSort list5
    , bench "250000 elements" $ whnf LL.mergeSort list6
    , bench "500000 elements" $ whnf LL.mergeSort list7
    , bench "750000 elements" $ whnf LL.mergeSort list8
    , bench "1000000 elements" $ whnf LL.mergeSort list9
    ]
  , bgroup
    "Linked List Filter"
    [ bench "10 elements" $ nf (LL.filter isOdd) list1
    , bench "100 elements" $ nf (LL.filter isOdd) list2
    , bench "1000 elements" $ nf (LL.filter isOdd) list3
    , bench "10000 elements" $ nf (LL.filter isOdd) list4
    , bench "100000 elements" $ nf (LL.filter isOdd) list5
    , bench "250000 elements" $ nf (LL.filter isOdd) list6
    , bench "500000 elements" $ nf (LL.filter isOdd) list7
    , bench "750000 elements" $ nf (LL.filter isOdd) list8
    , bench "1000000 elements" $ nf (LL.filter isOdd) list9
    ]
  , bgroup
    "Linked List Reduce"
    [ bench "10 elements" $ whnf (LL.reduce (+) 0) list1
    , bench "100 elements" $ whnf (LL.reduce (+) 0) list2
    , bench "1000 elements" $ whnf (LL.reduce (+) 0) list3
    , bench "10000 elements" $ whnf (LL.reduce (+) 0) list4
    , bench "100000 elements" $ whnf (LL.reduce (+) 0) list5
    , bench "250000 elements" $ whnf (LL.reduce (+) 0) list6
    , bench "500000 elements" $ whnf (LL.reduce (+) 0) list7
    , bench "750000 elements" $ whnf (LL.reduce (+) 0) list8
    , bench "1000000 elements" $ whnf (LL.reduce (+) 0) list9
    ]
  ]

main = do
  benchmark list1 list2 list3 list4 list5 list6 list7 list8 list9
  where
    list1 = (createList 1 10 LL.Empty)
    list2 = (createList 1 100 LL.Empty)
    list3 = (createList 1 1000 LL.Empty)
    list4 = (createList 1 10000 LL.Empty)
    list5 = (createList 1 100000 LL.Empty)
    list6 = (createList 1 250000 LL.Empty)
    list7 = (createList 1 500000 LL.Empty)
    list8 = (createList 1 750000 LL.Empty)
    list9 = (createList 1 1000000 LL.Empty)
