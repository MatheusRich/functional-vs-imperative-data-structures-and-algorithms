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

benchmark list1 list2 list3 list4 list5 list6 = defaultMain
  [ bgroup
    "Linked List Sort"
    [ bench "10 elements" $ whnf LL.mergeSort list1
    , bench "100 elements" $ whnf LL.mergeSort list2
    , bench "1 000 elements" $ whnf LL.mergeSort list3
    , bench "10 000 elements" $ whnf LL.mergeSort list4
    , bench "100 000 elements" $ whnf LL.mergeSort list5
    , bench "1 000 000 elements" $ whnf LL.mergeSort list6
    ]
  , bgroup
    "Linked List Filter"
    [ bench "10 elements" $ nf (LL.filter isOdd) list1
    , bench "100 elements" $ nf (LL.filter isOdd) list2
    , bench "1 000 elements" $ nf (LL.filter isOdd) list3
    , bench "10 000 elements" $ nf (LL.filter isOdd) list4
    , bench "100 000 elements" $ nf (LL.filter isOdd) list5
    , bench "1 000 000 elements" $ nf (LL.filter isOdd) list6
    ]
  , bgroup
    "Linked List Reduce"
    [ bench "10 elements" $ whnf (LL.reduce' (+) 0) list1
    , bench "100 elements" $ whnf (LL.reduce' (+) 0) list2
    , bench "1 000 elements" $ whnf (LL.reduce' (+) 0) list3
    , bench "10 000 elements" $ whnf (LL.reduce' (+) 0) list4
    , bench "100 000 elements" $ whnf (LL.reduce' (+) 0) list5
    , bench "1 000 000 elements" $ whnf (LL.reduce' (+) 0) list6
    ]
  ]

main = do
  let list1 = createList 0 10 LL.Empty
  let list2 = createList 0 100 LL.Empty
  let list3 = createList 0 1000 LL.Empty
  let list4 = createList 0 10000 LL.Empty
  let list5 = createList 0 100000 LL.Empty
  let list6 = createList 0 1000000 LL.Empty

  benchmark list1 list2 list3 list4 list5 list6
