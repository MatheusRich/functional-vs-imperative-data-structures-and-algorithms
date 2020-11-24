import qualified BinarySearchTree as BST

maybeToStr :: Show a => Maybe a -> String
maybeToStr (Just value) = show value
maybeToStr Nothing = "Fails"

assert :: Bool -> String -> IO ()
assert True msg = putStrLn msg
assert False _ = putStrLn "Fails"

main = do
  putStrLn "Creating a new binary search tree of integers"
  let emptyTree = BST.Empty
  assert (BST.isEmpty emptyTree) "It is empty"
  putStrLn "Pushing 3, -1, -4 and 5"
  let tree = BST.push 5 (BST.push (-4) (BST.push (-1) (BST.push 3 emptyTree)))
  assert (not (BST.isEmpty tree)) "It is not empty"
  putStrLn ("Depth is " ++ show (BST.depth tree))
  putStrLn ("First negative number is " ++ maybeToStr (BST.find (\x -> x < 0) tree))
