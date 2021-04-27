import Criterion.Main
import qualified BinarySearchTree as BST

-- (1..10).sort_by { Math.sqrt(_1).round(1).to_s.split('.').last.to_i
-- createTree :: Int -> BST.BinarySearchTree Int -> BST.BinarySearchTree Int
-- createTree 0    tree = tree
-- createTree size tree = createTree (size - 1) (BST.push (size * (-1) ^ size) tree)

criterion :: BST.BinarySearchTree Int -> BST.BinarySearchTree Int -> BST.BinarySearchTree Int -> IO ()
criterion list1 list2 list3 = defaultMain
  [ bgroup
    "BST contains"
    [ bench "10 000 elements" $ whnf BST.contains 9
    , bench "100 000 elements" $ whnf BST.contains 99
    , bench "1 000 000 elements" $ whnf BST.contains 999
    ]
  -- , bgroup
  --   "BST map"
  --   [ bench "10 000 elements" $ whnf (LL.map (\x -> x * 2)) list1
  --   , bench "100 000 elements" $ whnf (LL.map (\x -> x * 2)) list2
  --   , bench "1 000 000 elements" $ whnf (LL.map (\x -> x * 2)) list3
  --   ]
  -- , bgroup
  --   "BST Reduce"
  --   [ bench "10 000 elements" $ whnf (LL.reduce (+) 0) list1
  --   , bench "100 000 elements" $ whnf (LL.reduce (+) 0) list2
  --   , bench "1 000 000 elements" $ whnf (LL.reduce (+) 0) list3
  --   ]
  ]

main = do
  putStrLn "Bench for 10 000, 100 000 and 1 000 000 elements"
  let tree  = createTree 10000 BST.Empty
  let tree2 = createTree 100000 BST.Empty
  let tree3 = createTree 1000000 BST.Empty
  criterion tree tree2 tree3
