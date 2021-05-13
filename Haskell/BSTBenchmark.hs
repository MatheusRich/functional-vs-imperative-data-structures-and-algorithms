{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}

import           GHC.Generics (Generic)
import           Control.DeepSeq
import           Criterion.Main
import qualified BinarySearchTree as BST

deriving instance Show a => Show (BST.BinarySearchTree a)
deriving instance Generic (BST.BinarySearchTree a)
deriving instance NFData a => NFData (BST.BinarySearchTree a)

isBalanced :: BST.BinarySearchTree Int -> Bool
isBalanced BST.Empty = True
isBalanced (BST.Node head left right) =
  if ((abs (lh - rh)) <= 1) && (isBalanced left) && (isBalanced right)
    then True
    else False
  where
    lh = BST.depth (left)
    rh = BST.depth (right)


bstFromList :: [Int] -> BST.BinarySearchTree Int
bstFromList []   = BST.Empty
bstFromList list = (BST.Node mid) (bstFromList half0) (bstFromList half1)
  where
    (half0, half') = splitAt (((length list) `div` 2)) list
    half1          = tail half'
    mid            = head half'

doubleN x = x * 2

benchmark tree1 tree2 tree3 tree4 tree5 tree6 = defaultMain
  [ bgroup
    "BST contains"
    [ bench "10 elements" $ nf (BST.contains 10) tree1
    , bench "100 elements" $ nf (BST.contains 100) tree2
    , bench "1 000 elements" $ nf (BST.contains 1000) tree3
    , bench "10 000 elements" $ nf (BST.contains 10000) tree4
    , bench "100 000 elements" $ nf (BST.contains 100000) tree5
    , bench "1 000 000 elements" $ nf (BST.contains 1000000) tree6
    ]
  , bgroup
    "BST map"
    [ bench "10 elements" (nf (BST.map doubleN) tree1)
    , bench "100 elements" (nf (BST.map doubleN) tree2)
    , bench "1 000 elements" (nf (BST.map doubleN) tree3)
    , bench "10 000 elements" (nf (BST.map doubleN) tree4)
    , bench "100 000 elements" (nf (BST.map doubleN) tree5)
    , bench "1 000 000 elements" (nf (BST.map doubleN) tree6)
    ]
  ]

main = do
  -- putStrLn (show (tree1))
  -- putStrLn (show (isBalanced (tree1)))
  benchmark tree1 tree2 tree3 tree4 tree5 tree6
  where
    tree1 = (bstFromList [1 .. 10])
    tree2 = (bstFromList [1 .. 100])
    tree3 = (bstFromList [1 .. 1000])
    tree4 = (bstFromList [1 .. 10000])
    tree5 = (bstFromList [1 .. 100000])
    tree6 = (bstFromList [1 .. 1000000])
