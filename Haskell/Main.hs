import qualified LinkedList as LL

maybeToStr :: Show a => Maybe a -> String
maybeToStr (Just value) = show value
maybeToStr Nothing = "Fails"

assert :: Bool -> String -> IO ()
assert True msg = putStrLn msg
assert False _ = putStrLn "Fails"

main = do
  putStrLn "Creating a new linked list of integers"
  let list = LL.Empty
  assert (LL.isEmpty list) "It is empty"
  putStrLn "Pushing 3, 1, 4, 1, and 5"
  let newList = LL.push 5 (LL.push 1 (LL.push 4 (LL.push 1 (LL.push 3 list))))
  assert (not (LL.isEmpty newList)) "It is not empty"
  putStrLn ("Length is " ++ show (LL.length newList))
  putStrLn ("List is " ++ LL.toString (newList))
  putStrLn ("Head is " ++ maybeToStr (LL.head newList))
  putStrLn ("Tail is " ++ LL.toString (LL.tail newList))
  putStrLn ("Tail length is " ++ show (LL.length (LL.tail newList)))
  putStrLn ("Last is " ++ maybeToStr (LL.last newList))
  putStrLn ("Reversed list is " ++ LL.toString (LL.reverse newList))
  putStrLn ("List as string is " ++ LL.toString (LL.map (\x -> show x) newList))
  putStrLn ("List without even numbers is " ++ LL.toString (LL.filter (\x -> rem x 2 /= 0) newList))
  putStrLn ("Sum of all list elements is " ++ show (LL.reduce (+) 0 newList))
  putStrLn ("Sorted list is " ++ LL.toString (LL.sort newList))
