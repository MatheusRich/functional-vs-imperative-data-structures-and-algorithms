import           Criterion.Main
import qualified BinarySearchTree              as BST

bstFromList []   = BST.Empty
bstFromList list = (BST.Node mid) (bstFromList half0) (bstFromList half1)
  where
    (half0, half') = splitAt ((length list `div` 2) - 1) list
    half1          = tail half'
    mid            = head half'

doubleN x = x * 2

benchmark tree1 tree2 tree3 tree4 tree5 tree6 = defaultMain
  [ bgroup
    "BST contains"
    [ bench "10 elements" $ whnf (BST.contains 6) tree1
    , bench "100 elements" $ whnf (BST.contains 51) tree2
    , bench "1 000 elements" $ whnf (BST.contains 501) tree3
    , bench "10 000 elements" $ whnf (BST.contains 5001) tree4
    , bench "100 000 elements" $ whnf (BST.contains 50001) tree5
    , bench "1 000 000 elements" $ whnf (BST.contains 500001) tree6
    , bench "10 000 000 elements" $ whnf (BST.contains 5000001) tree5
    , bench "100 000 000 elements" $ whnf (BST.contains 50000001) tree6
    ]
  , bgroup
    "BST map"
    [ bench "1 000 elements" $ whnf (BST.map doubleN) tree1
    , bench "10 000 elements" $ whnf (BST.map doubleN) tree2
    , bench "100 000 elements" $ whnf (BST.map doubleN) tree3
    , bench "1 000 000 elements" $ whnf (BST.map doubleN) tree4
    , bench "10 000 000 elements" $ whnf (BST.map doubleN) tree5
    , bench "100 000 000 elements" $ whnf (BST.map doubleN) tree6
    ]
  ]

main = do
  let tree1 = bstFromList [1 .. 10]
  let tree2 = bstFromList [1 .. 100]
  let tree3 = bstFromList [1 .. 1000]
  let tree4 = bstFromList [1 .. 10000]
  let tree5 = bstFromList [1 .. 100000]
  let tree6 = bstFromList [1 .. 1000000]

  benchmark tree1 tree2 tree3 tree4 tree5 tree6
